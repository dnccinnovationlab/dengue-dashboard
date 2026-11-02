#!/usr/bin/env python3
"""
Daily DGHS dengue press-release -> Google Sheet (tab: daily_dengue_dghs).

For every press release found on the DGHS listing page it extracts:
  affected / death        national 24-hour admissions and deaths   (PDF page 1)
  dhaka_case              Dhaka metro 24-hour admissions           (metro table)
  dncc_case               sum of the DNCC hospitals' 24-hour admissions (PDF pages 2-3)
  dscc_case               dhaka_case - dncc_case
  *_death                 24-hour deaths = today's cumulative deaths - previous day's
                          cumulative deaths (the PDF only publishes cumulative deaths
                          for the metro/hospitals)
and upserts one row per date.

Usage
  python dengue_dghs_daily.py                      # latest DAYS_BACK days -> Google Sheet
  python dengue_dghs_daily.py --dry-run            # print rows, do not write
  python dengue_dghs_daily.py --pdf report.pdf     # parse one local PDF (for testing)
  python dengue_dghs_daily.py --dump report.pdf    # show extracted text per page
Env
  GOOGLE_SERVICE_ACCOUNT   service-account JSON (string)      [required to write]
  SHEET_ID                 default: sheet id below
  DAYS_BACK                default 3
"""
import argparse, datetime as dt, io, json, os, re, sys, time

import requests

LISTING_URL = ("https://dghs.gov.bd/pages/miscellaneous-infos?filters="
               "%7B%22miscellaneous_info_type%22%3A%226a9cf0471fa8cd87d1f50227%22%7D")
SHEET_ID = os.environ.get("SHEET_ID", "1EMva389XPbDyJs80M3FZhgj8hGNCOouWzo4bz0cr0Ww")
TAB = "daily_dengue_dghs"
HEADERS = ["date", "affected", "death", "dncc_case", "dncc_death",
           "dscc_case", "dscc_death", "dhaka_case", "dhaka_death"]
UA = {"User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
                    "(KHTML, like Gecko) Chrome/124.0 Safari/537.36",
      "Accept-Language": "bn,en;q=0.8"}

# --- which hospitals count as DNCC -------------------------------------------------
# Key = serial number inside the PDF list. List 1 = government/autonomous hospitals,
# list 2 = private hospitals (serials 1-23 on PDF page 2, 24-59 on page 3).
# Edit these sets if the report's hospital list changes.
DNCC_GOVT = {3, 4, 9, 10, 11, 12, 13, 18}
#            3 Shishu, 4 Suhrawardy, 9 CMH Dhaka, 10 Kurmitola, 11 Kuwait Maitree,
#            12 TB Shyamoli, 13 Infectious Disease, 18 DNCC COVID-19 hospital
DNCC_PRIVATE = {5, 9, 12, 13, 17, 19, 21, 23,          # page 2
                26, 27, 30, 32, 33, 34, 37, 39, 47,     # page 3
                52, 55, 56, 58, 59}

BN = str.maketrans("০১২৩৪৫৬৭৮৯", "0123456789")
NUM = r"[০-৯0-9]+"
HOSP_RE = re.compile(rf"^({NUM})\s+(.*\S)\s+({NUM})\s+({NUM})\s+({NUM})\s+({NUM})\s+({NUM})$")
ROW7_RE = re.compile(rf"^(?:{NUM}\s+){{6}}{NUM}$")
ROW6_RE = re.compile(rf"^(?:{NUM}\s+){{5}}{NUM}$")


def n(s):
    return int(s.translate(BN))


def log(*a):
    print(*a, file=sys.stderr)


# --- PDF parsing -------------------------------------------------------------------
def pdf_pages_text(data: bytes):
    import pdfplumber
    with pdfplumber.open(io.BytesIO(data)) as pdf:
        return [(p.extract_text() or "") for p in pdf.pages]


def parse_report(pages):
    """pages: list[str] (one per PDF page). Returns dict of figures."""
    lines = [[l.strip() for l in p.splitlines() if l.strip()] for p in pages]

    # national totals: first numbers-only 6-number line (page 1)
    nat = None
    for l in (lines[0] if lines else []):
        if ROW6_RE.match(l):
            nat = [n(x) for x in l.split()]
            break
    if not nat:
        raise ValueError("national total line not found on page 1")
    national = dict(new=nat[0], deaths_24h=nat[1], cum=nat[2], deaths_cum=nat[3])

    # Dhaka metro total: first numbers-only 7-number line (metro table, page 4)
    metro = None
    for pl in lines:
        for l in pl:
            if ROW7_RE.match(l):
                v = [n(x) for x in l.split()]
                if v[0] + v[1] == v[2]:      # govt + private == total
                    metro = v
                    break
        if metro:
            break
    if not metro:
        raise ValueError("Dhaka metro total line not found")
    metro = dict(new=metro[2], cum=metro[3], deaths_cum=metro[4],
                 discharged=metro[5], current=metro[6])

    # hospital rows on pages 2-3
    hosp, list_id, last = {}, 0, None
    for pl in lines[1:3]:
        for l in pl:
            m = HOSP_RE.match(l)
            if not m or not re.search(r"[^\d০-৯\s]", m.group(2)):
                continue
            s = n(m.group(1))
            if s == 1 and (last is None or last > 1):
                list_id += 1
            last = s
            vals = [n(m.group(i)) for i in range(3, 8)]  # new, cum, deaths, disch, current
            hosp.setdefault((list_id, s), vals)

    n_govt = sum(1 for k in hosp if k[0] == 1)
    n_priv = sum(1 for k in hosp if k[0] == 2)
    all_new = sum(v[0] for v in hosp.values())
    if (n_govt, n_priv) != (18, 59) or all_new != metro["new"]:
        log(f"WARNING hospital parse check: govt={n_govt}/18 private={n_priv}/59 "
            f"sum24h={all_new} vs metro={metro['new']}  -> check the PDF layout")

    sel = [v for (lid, s), v in hosp.items()
           if (lid == 1 and s in DNCC_GOVT) or (lid == 2 and s in DNCC_PRIVATE)]
    dncc = dict(new=sum(v[0] for v in sel), cum=sum(v[1] for v in sel),
                deaths_cum=sum(v[2] for v in sel), n_hospitals=len(sel))
    return dict(national=national, metro=metro, dncc=dncc)


# --- listing + download --------------------------------------------------------------
def get(url, **kw):
    last = None
    for i in range(4):
        try:
            r = requests.get(url, headers=UA, timeout=60, **kw)
            r.raise_for_status()
            return r
        except Exception as e:           # gov sites are flaky from cloud IPs
            last = e
            time.sleep(3 * (i + 1))
    raise last


def list_reports(url=LISTING_URL):
    """-> {date: pdf_url} from the DGHS listing page."""
    from bs4 import BeautifulSoup
    soup = BeautifulSoup(get(url).text, "html.parser")
    out = {}
    for tr in soup.find_all("tr"):
        a = next((x for x in tr.find_all("a", href=True) if x["href"].lower().endswith(".pdf")), None)
        if not a:
            continue
        txt = tr.get_text(" ", strip=True).translate(BN)
        m = re.search(r"\((\d{2})/(\d{2})/(\d{4})\)", txt) or re.search(r"(\d{2})-(\d{2})-(\d{4})", txt)
        if not m:
            continue
        d = dt.date(int(m.group(3)), int(m.group(2)), int(m.group(1)))
        out[d] = a["href"]
    return out


_cache = {}


def report_for(d, urls):
    if d not in urls:
        return None
    if d not in _cache:
        pages = pdf_pages_text(get(urls[d]).content)
        _cache[d] = parse_report(pages)
    return _cache[d]


def build_row(d, urls):
    cur = report_for(d, urls)
    if cur is None:
        return None
    prev = report_for(d - dt.timedelta(days=1), urls)
    nat, met, dn = cur["national"], cur["metro"], cur["dncc"]
    row = dict(date=d.isoformat(), affected=nat["new"], death=nat["deaths_24h"],
               dncc_case=dn["new"], dscc_case=met["new"] - dn["new"], dhaka_case=met["new"],
               dncc_death="", dscc_death="", dhaka_death="")
    if prev:
        dh = met["deaths_cum"] - prev["metro"]["deaths_cum"]
        dd = dn["deaths_cum"] - prev["dncc"]["deaths_cum"]
        row.update(dhaka_death=dh, dncc_death=dd, dscc_death=dh - dd)
    return row


# --- Google Sheet --------------------------------------------------------------------
def upsert(rows):
    import gspread
    from google.oauth2.service_account import Credentials
    creds = Credentials.from_service_account_info(
        json.loads(os.environ["GOOGLE_SERVICE_ACCOUNT"]),
        scopes=["https://www.googleapis.com/auth/spreadsheets"])
    ws = gspread.authorize(creds).open_by_key(SHEET_ID).worksheet(TAB)
    header = ws.row_values(1)
    missing = [h for h in HEADERS if h not in header]
    if missing:
        raise SystemExit(f"Sheet header is missing columns: {missing}")
    dates = ws.col_values(header.index("date") + 1)
    for r in rows:
        values = [r.get(h, "") for h in header]
        if r["date"] in dates:
            i = dates.index(r["date"]) + 1
            ws.update(range_name=f"A{i}", values=[values], value_input_option="USER_ENTERED")
            log("updated", r["date"], "row", i)
        else:
            ws.append_row(values, value_input_option="USER_ENTERED")
            dates.append(r["date"])
            log("appended", r["date"])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--pdf")
    ap.add_argument("--dump")
    ap.add_argument("--from-date"); ap.add_argument("--to-date")
    a = ap.parse_args()

    if a.dump:
        for i, t in enumerate(pdf_pages_text(open(a.dump, "rb").read()), 1):
            print(f"===== PAGE {i} =====\n{t}")
        return
    if a.pdf:
        print(json.dumps(parse_report(pdf_pages_text(open(a.pdf, "rb").read())),
                         indent=2, ensure_ascii=False))
        return

    urls = list_reports()
    if not urls:
        raise SystemExit("No press releases found on the listing page (blocked or layout changed?)")
    log("found", len(urls), "reports; latest", max(urls))
    if a.from_date or a.to_date:
        lo = dt.date.fromisoformat(a.from_date) if a.from_date else min(urls)
        hi = dt.date.fromisoformat(a.to_date) if a.to_date else max(urls)
        targets = sorted(d for d in urls if lo <= d <= hi)
    else:
        targets = sorted(urls)[-int(os.environ.get("DAYS_BACK", "3")):]
    rows = [r for r in (build_row(d, urls) for d in targets) if r]
    for r in rows:
        print(r)
    if rows and not a.dry_run:
        upsert(rows)


if __name__ == "__main__":
    main()

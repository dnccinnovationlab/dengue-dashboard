#!/usr/bin/env python3
"""
dncc_dengue_to_sheet.py
=======================
Downloads the daily DNCC dengue situation PDFs, extracts

  * the "Actual Dengue Patient list of Zone wise" table  -> sheet tab "patient"
  * the "Dengue Death Situation" list                     -> sheet tab "death"

and keeps both tabs in chronological order, one row per patient / death with
Total = 1.  Every calendar day between the first and the latest date that has
no data gets a single row with Total = 0.

Only reports dated AFTER the cut-off date (default 2026-10-18) are used - rows
up to that date are already in the sheet and are never touched.  Only the last
few days at the bottom of each tab are re-written, so older rows (and any manual
corrections / formatting in them) stay exactly as they are.

Typical use
-----------
  # daily automation (GitHub Actions, 17:30 Dhaka) - needs GOOGLE_SERVICE_ACCOUNT
  python dncc_dengue_to_sheet.py

  # test without touching Google Sheets (reads/writes patient.csv + death.csv)
  python dncc_dengue_to_sheet.py --pdf-dir ./pdfs --local-dir ./out --since 2026-10-18

  # only list the PDF links found on the DNCC listing page
  python dncc_dengue_to_sheet.py --discover-only

Sheet columns (matched to your existing header names; "Total" is added if missing)
-------------
  patient: Date | SNo | Name | Mobile | Address | Zone | Ward | Total
  death  : Date | Name | Hospital | Address | Hospital Inside DNCC |
           Residence Inside DNCC | Date Estimated | Total
           (only deaths whose "Residence Inside Deceased" is Yes are kept)
  (death "Date" = date of death; "Date Estimated" = Yes when the report
   gives no death date and the first report date is used instead)

Total = 1 for every patient / death, 0 for a day with no data.  Days between
two reports that have no report at all also get a Total = 0 row, so every date
is present, in order.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import json
import logging
import os
import re
import sys
import tempfile
from collections import OrderedDict, defaultdict
from pathlib import Path
from urllib.parse import parse_qsl, quote, urlencode, urljoin, urlparse, urlunparse

import pdfplumber
import requests

# --------------------------------------------------------------------------- #
# Configuration (override with environment variables)
# --------------------------------------------------------------------------- #
LISTING_URL = os.getenv(
    "DNCC_LISTING_URL",
    "https://dncc.gov.bd/pages/reports?filters="
    "%7B%22reports_type%22%3A%2269d2075cf94cbc83926de345%22%7D",
)
SHEET_ID = os.getenv("SHEET_ID", "164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg")
PATIENT_TAB = os.getenv("PATIENT_TAB", "patient")
DEATH_TAB = os.getenv("DEATH_TAB", "death")

MAX_PAGES = int(os.getenv("MAX_PAGES", "1"))      # listing pages to scan
PAGE_SIZE = int(os.getenv("PAGE_SIZE", "10"))     # listing page size
# Reports whose date is within the last N days (counted back from the newest
# date) are re-read and replace what is in the sheet; older dates already in
# the sheet are left untouched so manual corrections are not overwritten.
REFRESH_LAST_DAYS = int(os.getenv("REFRESH_LAST_DAYS", "1"))
# Only reports dated after this day are used (everything before is already in the sheet)
CUTOFF_DATE = os.getenv("CUTOFF_DATE", "2026-10-18")
# How many of the newest days at the bottom of each tab may be re-written
# (lets a late-uploaded report fill a "0" gap row).  Rows older than this are never touched.
TAIL_DAYS = int(os.getenv("TAIL_DAYS", "7"))

UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124 Safari/537.36"

PATIENT_HEADER = ["Date", "SNo", "Name", "Mobile", "Address", "Zone", "Ward", "Total"]
DEATH_HEADER = ["Date", "Name", "Hospital", "Address", "Hospital Inside DNCC",
                "Residence Inside DNCC", "Date Estimated", "Total"]

PATIENT_TITLE = "Actual Dengue Patient list of Zone wise"
PATIENT_STOP = "Urban Primary Health Care"
DEATH_START = "Dengue Death Situation"
DEATH_END = "Total Death at DNCC Area"

# x-coordinates (PDF points) of the patient table columns - identical in every
# report: Name < 180 <= Mobile < 262 <= Address < 500 <= Zone < 540 <= Ward
X_MOBILE, X_ADDRESS, X_ZONE, X_WARD = 180, 262, 500, 540

log = logging.getLogger("dncc")

MONTHS = {m: i for i, m in enumerate(
    ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"], 1)}


# --------------------------------------------------------------------------- #
# 1. Finding and downloading the PDFs
# --------------------------------------------------------------------------- #
def with_page(url: str, page: int, size: int) -> str:
    """Return the listing URL with page / page_size query parameters set."""
    p = urlparse(url)
    q = dict(parse_qsl(p.query, keep_blank_values=True))
    q["page"], q["page_size"] = str(page), str(size)
    return urlunparse(p._replace(query=urlencode(q, quote_via=quote)))


_PDF_RE = re.compile(r"""[^\s"'<>()\\]+?\.pdf(?:\?[^\s"'<>()\\]*)?""", re.I)


def pdf_links_in_text(text: str, base: str) -> list[str]:
    """Find every PDF URL in html / json text (handles JSON-escaped slashes)."""
    text = text.replace("\\/", "/").replace("\\u0026", "&").replace("&amp;", "&")
    out = []
    for m in re.finditer(r"""(?:href|src|data)\s*=\s*["']([^"']+\.pdf[^"']*)["']""", text, re.I):
        out.append(urljoin(base, m.group(1).strip().replace(" ", "%20")))   # names with spaces
    for m in _PDF_RE.finditer(text):
        link = m.group(0).strip().lstrip("=:,[{")
        if link.startswith("//"):
            link = "https:" + link
        out.append(urljoin(base, link))
    out = list(OrderedDict.fromkeys(out))
    # drop fragments produced by file names that contain spaces
    return [c for c in out
            if not any(o != c and o.endswith(c.rsplit("/", 1)[-1]) and
                       len(o.rsplit("/", 1)[-1]) > len(c.rsplit("/", 1)[-1]) for o in out)]


def discover_static(url: str) -> list[str]:
    try:
        r = requests.get(url, headers={"User-Agent": UA}, timeout=60)
        log.info("GET %s -> HTTP %s (%d bytes)", url, r.status_code, len(r.content))
        if r.status_code != 200:
            return []
        return pdf_links_in_text(r.text, url)
    except requests.RequestException as e:
        log.warning("static fetch failed: %s", e)
        return []


def discover_browser(url: str) -> list[str]:
    """Render the page in headless Chromium (the listing is probably a JS app)
    and collect PDF links from the DOM and from every JSON/HTML response."""
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        log.warning("playwright not installed - browser discovery skipped")
        return []
    links: list[str] = []
    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_context(user_agent=UA).new_page()

        def on_response(resp):
            try:
                if resp.url.lower().split("?")[0].endswith(".pdf"):
                    links.append(resp.url)
                ctype = resp.headers.get("content-type", "")
                if "json" in ctype or "html" in ctype or "javascript" in ctype:
                    links.extend(pdf_links_in_text(resp.text(), resp.url))
            except Exception:  # noqa: BLE001 - body not available for some responses
                pass

        page.on("response", on_response)
        page.goto(url, wait_until="networkidle", timeout=90_000)
        page.wait_for_timeout(3000)
        links.extend(pdf_links_in_text(page.content(), url))
        for sel, attr in (("a[href]", "href"), ("iframe[src]", "src"),
                          ("embed[src]", "src"), ("object[data]", "data")):
            for v in page.eval_on_selector_all(sel, f"els => els.map(e => e.getAttribute('{attr}'))"):
                if v and ".pdf" in v.lower():
                    links.append(urljoin(url, v))
        browser.close()
    return list(OrderedDict.fromkeys(links))


def discover_pdfs(max_pages: int) -> list[str]:
    found: list[str] = []
    for page_no in range(1, max_pages + 1):
        url = with_page(LISTING_URL, page_no, PAGE_SIZE)
        links = discover_static(url)
        if not links:
            log.info("no PDF links in static HTML - trying headless browser")
            links = discover_browser(url)
        log.info("page %d: %d PDF link(s)", page_no, len(links))
        new = [l for l in links if l not in found]
        if not new:
            break
        found.extend(new)
    return found


def download_pdf(url: str, dest_dir: Path) -> Path | None:
    name = re.sub(r"[^A-Za-z0-9._-]", "_", Path(urlparse(url).path).name) or "report.pdf"
    dest = dest_dir / name
    try:
        r = requests.get(url, headers={"User-Agent": UA}, timeout=120)
        r.raise_for_status()
    except requests.RequestException as e:
        log.warning("download failed %s: %s", url, e)
        return None
    if not r.content.startswith(b"%PDF"):
        log.warning("not a PDF: %s", url)
        return None
    dest.write_bytes(r.content)
    return dest


# --------------------------------------------------------------------------- #
# 2. Reading the PDFs
# --------------------------------------------------------------------------- #
def parse_date_text(day: str, month: str, year: str | int) -> dt.date | None:
    m = MONTHS.get(month.strip(". ").lower()[:3])
    if not m:
        return None
    try:
        return dt.date(int(year), m, int(day))
    except ValueError:
        return None


def report_date(first_page_text: str) -> dt.date | None:
    m = re.search(r"Date:\s*(\d{1,2})\s+([A-Za-z]+)\.?,?\s+(\d{4})", first_page_text)
    return parse_date_text(*m.groups()) if m else None


_MOBILE_PHONE = re.compile(r'^[\d\-+."]+(E\+\d+)?$')
_HASH = re.compile(r"#{3,}")


def _phoneish(tok: str) -> bool:
    t = tok.strip('"')
    return bool(_MOBILE_PHONE.match(tok)) and (len(t) >= 5 or t == "0")


def extract_patients(pdf: pdfplumber.PDF, texts: list[str]) -> tuple[list[dict], str | None]:
    """Patient table, read by column position (very reliable for these PDFs).
    Returns (rows, stated_count_from_header)."""
    stated = None
    full = "\n".join(texts)
    m = re.search(r"DNCC Patient-\s*(\S+)", full)
    if m:
        stated = m.group(1)

    starts = [i for i, t in enumerate(texts) if PATIENT_TITLE in t]
    if not starts:
        return [], stated
    start = starts[0]
    stops = [i for i, t in enumerate(texts) if PATIENT_STOP in t and i >= start]
    end = stops[0] if stops else len(texts) - 1

    rows: list[tuple[int, dict]] = []     # kept in printed order, nothing is dropped
    cur = None
    last_sno = 0
    for pg in range(start, end + 1):
        page = pdf.pages[pg]
        words = page.extract_words()
        top_min, top_max = 0.0, 1e9
        if pg == start:
            hits = page.search(PATIENT_TITLE)
            if hits:
                top_min = hits[0]["bottom"]
        if pg == end:
            urban = [w for w in words if w["text"] == "Urban"]
            if urban:
                top_max = min(w["top"] for w in urban) - 1
        words = sorted((w for w in words if top_min < w["top"] < top_max),
                       key=lambda w: (w["top"], w["x0"]))
        lines: list[list] = []
        for w in words:
            if lines and abs(w["top"] - lines[-1][0]) <= 2:
                lines[-1][1].append(w)
            else:
                lines.append([w["top"], [w]])
        for _, lw in lines:
            lw.sort(key=lambda w: w["x0"])
            if any(w["text"] == "S.No" for w in lw):       # repeated header row
                continue
            for w in lw:
                t, x = w["text"], w["x0"]
                if "(cid:" in t or any("\u0980" <= c <= "\u09ff" for c in t):
                    continue                                 # Bengali summary note
                if x < 60 and re.fullmatch(r"\d{1,3}", t):
                    sno = int(t)
                    if sno <= last_sno:      # typo in the report (e.g. "80" printed twice)
                        log.warning("serial no. %d repeated/out of order after %d - renumbered %d",
                                    sno, last_sno, last_sno + 1)
                        sno = last_sno + 1
                    last_sno = sno
                    cur = {"Name": [], "Band": [], "Address": [], "Zone": [], "Hash": False}
                    rows.append((sno, cur))
                    continue
                if cur is None or re.fullmatch(r"=+", t):
                    continue
                if _HASH.fullmatch(t):
                    cur["Hash"] = True
                elif x < X_MOBILE:
                    cur["Name"].append(t)
                elif x < X_ADDRESS:
                    cur["Band"].append(t)
                elif x < X_ZONE:
                    cur["Address"].append(t)
                elif x < X_WARD:
                    cur["Zone"].append(t)

    out = []
    for sno, c in rows:
        band = c["Band"]
        phones = [t for t in band if _phoneish(t)]
        if phones:
            mobile = ""
            for t in phones:
                t = t.replace('"', "")
                mobile = t if not mobile else (mobile + t if mobile.endswith("-") else f"{mobile} {t}")
            addr_front = [t for t in band if not _phoneish(t)]
        else:
            mobile, addr_front = " ".join(band), []
        if c["Hash"]:
            mobile = "##########"
        out.append({
            "SNo": sno,
            "Name": " ".join(c["Name"]).strip(" ,"),
            "Mobile": mobile.strip(),
            "Address": " ".join(addr_front + c["Address"]).strip(" ,"),
            "Zone": "".join(c["Zone"]).strip(),
            "Ward": "",
        })
    return out, stated


_FIG_RE = re.compile(r"Fig-\d+:[^\n]*?\(Real-Time PCR\)")
_NUMERIC_LINE = re.compile(r"^[\d\s,.%]+$")


def extract_deaths(texts: list[str], rep_date: dt.date) -> list[dict]:
    """Death list: Name / hospital / address / date of death / inside-DNCC flags."""
    full = "\n".join(texts)
    s = full.find(DEATH_START)
    if s < 0:
        return []
    e = full.find(DEATH_END, s)
    seg = full[s:e if e > 0 else len(full)]

    seg_lines = seg.split("\n")
    # the header block holds a lone number (country-wide death total) that the PDF
    # sometimes glues onto a hospital/address line - find it so it can be removed
    total_tok = None
    for raw in seg_lines:
        if re.match(r"^\d{1,3}\s+Name:", raw.strip()):
            break
        if re.fullmatch(r"\d{2,4}", raw.strip()):
            total_tok = raw.strip()
    entries, cur = [], None
    for raw in seg_lines:
        line = _FIG_RE.sub("", raw).strip()     # chart title sometimes lands mid-list
        if not line:
            continue
        m = re.match(r"^(\d{1,3})\s+Name:\s*(.*)$", line)
        if m:
            if cur:
                entries.append(cur)
            cur = {"SL": int(m.group(1)), "Name": m.group(2).strip(), "Hospital": "",
                   "Address": "", "DateRaw": "", "Flags": ("", ""), "stage": "name"}
            continue
        if cur is not None and total_tok and not line.startswith("Date:"):
            line = re.sub(rf"(?<!\S){total_tok}(?!\S)", " ", line).strip()
            if not line:
                continue
        if cur is not None and not line.startswith(("Address:", "Date:")) and "Address:" in line:
            head, _, tail = line.partition("Address:")     # "<hospital> Address: <addr>"
            line = head.strip()
            fm0 = re.search(r"\b(Yes|No)\s+(Yes|No)\s*$", line)
            if fm0:
                cur["Flags"] = (fm0.group(1), fm0.group(2))
                line = line[:fm0.start()].strip()
            if line and not cur["Hospital"]:
                cur["Hospital"] = line
            cur["Address"], cur["stage"] = tail.strip(), "address"
            continue
        if cur is None or _NUMERIC_LINE.match(line) or line.startswith(("Upto", "Jan.")):
            continue                              # header text / chart labels
        fm = re.search(r"\b(Yes|No)\s+(Yes|No)\s*$", line)
        if fm:
            cur["Flags"] = (fm.group(1), fm.group(2))
            line = line[:fm.start()].strip()
        if line.startswith("Address:"):
            cur["Address"], cur["stage"] = line[8:].strip(), "address"
        elif line.startswith("Date:"):
            cur["DateRaw"], cur["stage"] = line[5:].strip(), "date"
        elif line:
            if cur["stage"] == "address":
                cur["Address"] += " " + line
            elif not cur["Hospital"]:
                cur["Hospital"] = line
            elif cur["stage"] == "name":
                cur["Hospital"] += " " + line
    if cur:
        entries.append(cur)
    if entries and len(entries) != max(en["SL"] for en in entries):
        log.warning("death list: %d entries read but serial numbers run to %d",
                    len(entries), max(en["SL"] for en in entries))

    out = []
    for en in entries:
        d = None
        m = re.match(r"(\d{1,2})\s+([A-Za-z]+)\.?,?\s*(\d{4})?", en["DateRaw"])
        if m:
            d = parse_date_text(m.group(1), m.group(2), m.group(3) or rep_date.year)
        out.append({
            "Name": en["Name"], "Hospital": en["Hospital"], "Address": en["Address"],
            "Date": d, "HospitalInside": en["Flags"][0], "ResidenceInside": en["Flags"][1],
        })
    return out


def read_report(pdf_path: Path) -> dict | None:
    try:
        with pdfplumber.open(pdf_path) as pdf:
            texts = [p.extract_text() or "" for p in pdf.pages]
            rdate = report_date(texts[0]) if texts else None
            if not rdate:
                log.warning("%s: no report date found - skipped", pdf_path.name)
                return None
            patients, stated = extract_patients(pdf, texts)
            deaths = extract_deaths(texts, rdate)
    except Exception as e:  # noqa: BLE001
        log.warning("%s: could not be read (%s)", pdf_path.name, e)
        return None
    flag = ""
    if stated and stated.isdigit() and int(stated) != len(patients):
        flag = f"  !! header says {stated}"
    log.info("%s: %s  patients=%d%s  deaths listed=%d", pdf_path.name, rdate,
             len(patients), flag, len(deaths))
    return {"date": rdate, "patients": patients, "deaths": deaths, "file": pdf_path.name}


# --------------------------------------------------------------------------- #
# 3. Reading the existing sheet
# --------------------------------------------------------------------------- #
def _norm_header(h) -> str:
    return re.sub(r"[^a-z0-9]", "", str(h).lower())


def _cell_str(v) -> str:
    if v is None:
        return ""
    if isinstance(v, float) and v.is_integer():
        return str(int(v))
    return str(v).strip()


def _is_num(v) -> bool:
    return isinstance(v, (int, float)) and not isinstance(v, bool)


def _to_date(v) -> dt.date | None:
    if _is_num(v):                                    # Google serial date number
        return dt.date(1899, 12, 30) + dt.timedelta(days=int(v)) if v > 30000 else None
    s = str(v).strip()
    for fmt in ("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%m/%d/%Y"):
        try:
            return dt.datetime.strptime(s, fmt).date()
        except ValueError:
            pass
    return None


class Table:
    """What is currently in a sheet tab."""

    def __init__(self, values: list[list], canon_header: list[str]):
        self.canon = canon_header
        self.n_values = len(values)
        self.header = [str(h) for h in values[0]] if values and any(str(c).strip() for c in values[0]) else []
        self.rows: list[dict] = []
        if not self.header:
            return
        names = {_norm_header(c): c for c in canon_header}
        cols = {i: names[_norm_header(h)] for i, h in enumerate(self.header) if _norm_header(h) in names}
        if "Date" not in cols.values():
            if len(values) > 1:
                sys.exit("The tab has data but no 'Date' column - refusing to touch it.")
            self.header = []
            return
        for rown, raw in enumerate(values[1:], start=2):
            f = {c: "" for c in canon_header}
            rawv = {}
            for i, c in cols.items():
                if i < len(raw):
                    f[c] = _cell_str(raw[i])
                    rawv[c] = raw[i]
            if not any(f.values()):
                continue
            real = bool(f.get("Name") or f.get("Address") or f.get("Mobile"))
            self.rows.append({"row": rown, "date": _to_date(rawv.get("Date", "")), "f": f,
                              "raw": rawv, "real": real})
        if self.rows and not any(r["date"] for r in self.rows):
            sys.exit("Could not read any date in the 'Date' column - refusing to touch the tab.")

    def style(self, col: str) -> str:
        """'num' / 'text' - how existing cells of this column are stored."""
        for r in reversed(self.rows[-40:]):
            v = r["raw"].get(col, "")
            if v not in ("", None):
                return "num" if _is_num(v) else "text"
        return "default"


# --------------------------------------------------------------------------- #
# 4. Storage back-ends (Google Sheet or local CSV)
# --------------------------------------------------------------------------- #
class GoogleSheetIO:
    sheet_syntax = True

    def __init__(self, sheet_id: str):
        import gspread
        raw = os.getenv("GOOGLE_SERVICE_ACCOUNT", "").strip()
        if raw:
            gc = gspread.service_account_from_dict(json.loads(raw))
        elif os.getenv("GOOGLE_APPLICATION_CREDENTIALS"):
            gc = gspread.service_account(filename=os.environ["GOOGLE_APPLICATION_CREDENTIALS"])
        else:
            sys.exit("Set GOOGLE_SERVICE_ACCOUNT (service-account JSON) or use --local-dir.")
        self.book = gc.open_by_key(sheet_id)

    def _ws(self, tab: str):
        import gspread
        try:
            return self.book.worksheet(tab)
        except gspread.WorksheetNotFound:
            log.info("creating tab '%s'", tab)
            return self.book.add_worksheet(title=tab, rows=1000, cols=12)

    def read(self, tab: str) -> list[list]:
        # UNFORMATTED -> real numbers / date serials, no locale-dependent text
        return self._ws(tab).get_all_values(value_render_option="UNFORMATTED_VALUE")

    def write_tail(self, tab, header, header_changed, first_row, rows, old_len):
        from gspread.utils import rowcol_to_a1
        ws = self._ws(tab)
        end_row = first_row + len(rows) - 1
        if ws.row_count < end_row:
            ws.add_rows(end_row - ws.row_count + 100)
        if ws.col_count < len(header):
            ws.add_cols(len(header) - ws.col_count)
        if header_changed:
            ws.update(values=[header], range_name="A1", value_input_option="RAW")
        for i in range(0, len(rows), 5000):
            ws.update(values=rows[i:i + 5000], range_name=f"A{first_row + i}",
                      value_input_option="USER_ENTERED")
        if old_len > end_row:                       # leftover (cannot happen unless rows shrank)
            ws.batch_clear([f"A{end_row + 1}:{rowcol_to_a1(old_len, len(header))}"])
        log.info("'%s': wrote %d row(s) at rows %d-%d", tab, len(rows), first_row, end_row)


class LocalCsvIO:
    """Same interface, but keeps patient.csv / death.csv in a folder (for testing)."""
    sheet_syntax = False

    def __init__(self, folder: str):
        self.dir = Path(folder)
        self.dir.mkdir(parents=True, exist_ok=True)

    def read(self, tab: str) -> list[list]:
        f = self.dir / f"{tab}.csv"
        if not f.exists():
            return []
        with f.open(encoding="utf-8-sig", newline="") as fh:
            return [list(r) for r in csv.reader(fh)]

    def write_tail(self, tab, header, header_changed, first_row, rows, old_len):
        values = self.read(tab)
        kept = values[:first_row - 1] if values else []
        if not kept:
            kept = [header]
        elif header_changed:
            kept[0] = header
        kept = [r + [""] * (len(header) - len(r)) for r in kept]
        with (self.dir / f"{tab}.csv").open("w", encoding="utf-8-sig", newline="") as fh:
            csv.writer(fh).writerows(kept + rows)
        log.info("'%s': wrote %d row(s) at rows %d-%d", tab, len(rows), first_row,
                 first_row + len(rows) - 1)


# --------------------------------------------------------------------------- #
# 5. Building the new bottom part of each tab
# --------------------------------------------------------------------------- #
def calendar(start: dt.date, end: dt.date):
    d = start
    while d <= end:
        yield d
        d += dt.timedelta(days=1)


def tail_window(table: Table, since: dt.date):
    """Returns (tail_from, first_tail_row, existing_tail_rows, last_head_date)."""
    dated = [r["date"] for r in table.rows if r["date"]]
    last = max(dated) if dated else None
    tail_from = since + dt.timedelta(days=1)
    if last:
        tail_from = max(tail_from, last - dt.timedelta(days=TAIL_DAYS - 1))
    tail = [r for r in table.rows if r["date"] and r["date"] >= tail_from]
    first_row = min((r["row"] for r in tail), default=table.n_values + 1)
    for r in table.rows:                       # rows below first_row must all be tail rows
        if r["row"] >= first_row and not (r["date"] and r["date"] >= tail_from):
            sys.exit(f"Row {r['row']} is not in chronological order - fix the sheet first.")
    head = [r["date"] for r in table.rows if r["date"] and r["date"] < tail_from]
    return tail_from, first_row, tail, (max(head) if head else None), last


def plan_patients(table: Table, reports: list[dict], since: dt.date, force: bool):
    tail_from, first_row, tail, last_head, last = tail_window(table, since)
    by_date: "dict[dt.date, list[dict]]" = defaultdict(list)
    for r in tail:
        if r["real"]:
            by_date[r["date"]].append(r["f"])
    report_dates = [rp["date"] for rp in reports]
    latest = max([d for d in [last, *report_dates] if d])
    changed = False
    for rp in sorted(reports, key=lambda x: x["date"]):
        d = rp["date"]
        if d < tail_from:
            (log.debug if any(r["date"] == d and r["real"] for r in table.rows) else log.warning)(
                "report %s is older than the editable window (%s) - not used", d, tail_from)
            continue
        refresh = force or d > latest - dt.timedelta(days=REFRESH_LAST_DAYS)
        if rp["patients"] and (d not in by_date or refresh):
            by_date[d] = [{"Date": d.isoformat(), **p} for p in rp["patients"]]
            changed = True
            log.info("patients %s: %d row(s)", d, len(rp["patients"]))
        elif not rp["patients"]:
            changed = changed or d > (last or since)
    start = (last_head + dt.timedelta(days=1)) if last_head else tail_from
    out = []
    for d in calendar(min(start, tail_from), latest):
        if by_date.get(d):
            for f in sorted(by_date[d], key=lambda f: int(f["SNo"]) if str(f["SNo"]).isdigit() else 10**6):
                out.append({**f, "Date": d, "Total": 1})
        else:
            out.append({"Date": d, "Total": 0})
    return {"first_row": first_row, "rows": out, "changed": changed or not tail}


def plan_deaths(table: Table, reports: list[dict], since: dt.date):
    real_existing = [r for r in table.rows if r["real"]]
    bootstrap = not real_existing                       # empty tab: take the whole list once
    existing_keys = {(_dkey(r["f"]["Name"]), r["date"]) for r in real_existing if r["date"]}
    all_new: list[dict] = []
    for rp in sorted(reports, key=lambda x: x["date"]):
        for d in rp["deaths"]:
            if d["ResidenceInside"].strip().lower() != "yes":     # keep DNCC residents only
                if not d["ResidenceInside"].strip():
                    log.warning("death '%s': 'Residence Inside Deceased' flag not read - skipped",
                                d["Name"])
                continue
            dd, est = d["Date"] or rp["date"], d["Date"] is None
            k = (_dkey(d["Name"]), dd)
            if k in existing_keys or any(n["Name"] == d["Name"] and n["Address"] == d["Address"]
                                         for n in all_new):
                continue
            if not bootstrap and dd <= since:
                log.warning("death dated %s (%s) is not in the sheet but is on/before the "
                            "cut-off - add it by hand if needed", dd, d["Name"])
                continue
            all_new.append({"Date": dd, "Name": d["Name"], "Hospital": d["Hospital"],
                            "Address": d["Address"],
                            "Hospital Inside DNCC": d["HospitalInside"],
                            "Residence Inside DNCC": d["ResidenceInside"],
                            "Date Estimated": "Yes" if est else "", "Total": 1})
            log.info("new death: %s (%s)", d["Name"], dd)
    dates = [n["Date"] for n in all_new]
    since_eff = since if not bootstrap else (min(dates) - dt.timedelta(days=1) if dates else since)
    tail_from, first_row, tail, last_head, last = tail_window(table, since_eff)
    if bootstrap and dates:
        tail_from = min(dates)
    by_date: "dict[dt.date, list[dict]]" = defaultdict(list)
    for r in tail:
        if r["real"]:
            by_date[r["date"]].append({**r["f"], "Date": r["date"], "Total": 1})
    for n in all_new:
        if n["Date"] >= tail_from:
            by_date[n["Date"]].append(n)
    latest = max([d for d in [last, *[rp["date"] for rp in reports], *dates] if d])
    start = (last_head + dt.timedelta(days=1)) if last_head else tail_from
    out = []
    for d in calendar(min(start, tail_from), latest):
        out.extend(by_date[d] if by_date.get(d) else [{"Date": d, "Total": 0}])
    return {"first_row": first_row, "rows": out, "changed": bool(all_new) or not tail}


def _dkey(name: str) -> str:
    return re.sub(r"[^a-z0-9]", "", name.lower())


# --------------------------------------------------------------------------- #
# 6. Writing
# --------------------------------------------------------------------------- #
def render(table: Table, canon_header: list[str], plan: dict, io) -> tuple[list, bool, list[list]]:
    """Lay the rows out under the sheet's own header (adds missing columns at the end)."""
    header = list(table.header) if table.header else list(canon_header)
    have = {_norm_header(h) for h in header}
    header_changed = not table.header
    for c in canon_header:
        if _norm_header(c) not in have:
            header.append(c)
            header_changed = True
            log.warning("column '%s' was not in the sheet - added at the end", c)
    canon_of = {_norm_header(c): c for c in canon_header}
    cols = [canon_of.get(_norm_header(h)) for h in header]
    styles = {c: table.style(c) for c in ("Date", "SNo", "Mobile", "Zone", "Ward")}

    def fmt(col, v):
        if col is None or v in ("", None):
            return ""
        if col == "Total":
            return int(v)
        if col == "Date":
            iso = v.isoformat()
            return "'" + iso if io.sheet_syntax and styles["Date"] == "text" else iso
        if col == "SNo":
            return int(v) if str(v).isdigit() else str(v)
        v = str(v)
        if io.sheet_syntax:
            if col in ("Mobile", "Zone", "Ward"):
                if styles[col] == "num" and v.isdigit():
                    return int(v)
                return "'" + v
            if v[0] in "=+-@":
                return "'" + v
        return v

    rows = [[fmt(c, r.get(c, "")) for c in cols] for r in plan["rows"]]
    return header, header_changed, rows


# --------------------------------------------------------------------------- #
# 7. Main
# --------------------------------------------------------------------------- #
def filename_date(name: str) -> dt.date | None:
    m = re.search(r"(\d{1,2})[_.\-](\d{1,2})[_.\-](\d{4}|\d{2})(?!\d)", name)
    if not m:
        return None
    d, mo, y = int(m.group(1)), int(m.group(2)), int(m.group(3))
    try:
        return dt.date(y + 2000 if y < 100 else y, mo, d)
    except ValueError:
        return None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--since", default=CUTOFF_DATE, help="only use reports AFTER this date (YYYY-MM-DD)")
    ap.add_argument("--pdf-dir", help="use PDFs from this folder instead of downloading")
    ap.add_argument("--local-dir", help="read/write patient.csv + death.csv here instead of Google Sheets")
    ap.add_argument("--max-pages", type=int, default=MAX_PAGES, help="listing pages to scan")
    ap.add_argument("--discover-only", action="store_true", help="print PDF links and exit")
    ap.add_argument("--force", action="store_true", help="re-write every date found in the PDFs")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO,
                        format="%(asctime)s %(levelname)s %(message)s", datefmt="%H:%M:%S")
    since = dt.date.fromisoformat(args.since)

    # ---- get the PDFs ---------------------------------------------------
    work = Path(tempfile.mkdtemp(prefix="dncc_"))
    if args.pdf_dir:
        pdfs = [p for p in sorted(Path(args.pdf_dir).glob("*.pdf"))
                if not (filename_date(p.name) and filename_date(p.name) <= since)]
    else:
        links = discover_pdfs(args.max_pages)
        for l in links:
            print(l)
        if args.discover_only:
            return 0 if links else 1
        if not links:
            log.error("No PDF links found on the DNCC page. Run with --discover-only -v; "
                      "the page layout may need a tweak (see README notes).")
            return 1
        pdfs = []
        for l in links:
            fd = filename_date(Path(urlparse(l).path).name)
            if fd and fd <= since:
                log.debug("skip %s (dated %s, not after %s)", l, fd, since)
                continue
            p = download_pdf(l, work)
            if p:
                pdfs.append(p)
    log.info("%d PDF(s) to read (reports after %s)", len(pdfs), since)

    reports: "dict[dt.date, dict]" = {}
    for p in pdfs:
        rp = read_report(p)
        if not rp or rp["date"] <= since:
            continue
        if rp["date"] not in reports or len(rp["patients"]) > len(reports[rp["date"]]["patients"]):
            reports[rp["date"]] = rp
    if not reports:
        log.info("No new report after %s yet - nothing to do.", since)
        return 0
    rep_list = list(reports.values())

    # ---- update the two tabs ---------------------------------------------
    io = LocalCsvIO(args.local_dir) if args.local_dir else GoogleSheetIO(SHEET_ID)

    for tab, canon, planner in (
        (PATIENT_TAB, PATIENT_HEADER, lambda t: plan_patients(t, rep_list, since, args.force)),
        (DEATH_TAB, DEATH_HEADER, lambda t: plan_deaths(t, rep_list, since)),
    ):
        table = Table(io.read(tab), canon)
        plan = planner(table)
        header, header_changed, rows = render(table, canon, plan, io)
        if not plan["rows"]:
            continue
        io.write_tail(tab, header, header_changed, plan["first_row"], rows, table.n_values)
    log.info("done")
    return 0


if __name__ == "__main__":
    sys.exit(main())

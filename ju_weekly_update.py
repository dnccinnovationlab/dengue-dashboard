#!/usr/bin/env python3
"""
ju_weekly_update.py

Finds new JU-DNCC weekly mosquito-surveillance PDFs on the DNCC reports page,
extracts Tables 1, 2, 3 and 5, and appends the rows to three tabs of a Google Sheet.

Tabs / columns (matched to the sheet's own header row by name, so column order
in the sheet does not matter):

  ju_adult_mosquito : Zone, Ae.aegypti, Ae.albopictus, Cx.quinquefasciatus, type, week_ju
                  type = "Light" (Table 1, Moshar CO2 trap) or "Gravid" (Table 3)
  ju_larvae         : zone, Ae.aegypti, Ae.albopictus, week_ju
  ju_bi_ci          : zone, Total_House, Positive_House, Total_Wet_container,
                  Positive_Wet_Container, BI, CI, HI, week_ju

Rules carried over from the earlier manual compilation:
  * Ae.albopictus missing from Table 1 -> 0
  * Gravid rows only when Table 3 really is the Gravid-trap table. From week 122
    the report's Table 3 became "Den-X-Trap eggs and larvae"; no Gravid rows are
    written for such weeks (nothing is invented).
  * A cell that is blank in the PDF stays blank in the sheet.

Idempotent: a week/type already present in a tab is skipped, so re-running is safe.
Only weeks > LAST_WEEK_DONE (default 123) are processed.

Usage
  python ju_weekly_update.py                 # normal run (used by GitHub Actions)
  python ju_weekly_update.py --pdf FILE.pdf  # parse one local PDF and print rows (no sheet access)
  python ju_weekly_update.py --dry-run       # discover + parse, but do not write to the sheet
"""

import argparse
import json
import logging
import os
import re
import subprocess
import sys
import tempfile
import time
from urllib.parse import quote, urljoin

import pdfplumber
import requests

# ----------------------------------------------------------------------------
# CONFIG (override with environment variables)
# ----------------------------------------------------------------------------
BASE_URL = "https://dncc.gov.bd"
REPORTS_TYPE_ID = os.getenv("REPORTS_TYPE_ID", "69a6621af681a7e27286ba8c")
PAGE_SIZE = int(os.getenv("PAGE_SIZE", "100"))
MAX_LIST_PAGES = int(os.getenv("MAX_LIST_PAGES", "5"))
LAST_WEEK_DONE = int(os.getenv("LAST_WEEK_DONE", "123"))

SHEET_ID = os.getenv("SHEET_ID", "164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg")
TAB_LIGHT_GRAVID = os.getenv("TAB_LIGHT_GRAVID", "ju_adult_mosquito")
TAB_TABLE2 = os.getenv("TAB_TABLE2", "ju_larvae")
TAB_TABLE5 = os.getenv("TAB_TABLE5", "ju_bi_ci")

USER_AGENT = os.getenv(
    "USER_AGENT",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/124.0 Safari/537.36 DNCC-weekly-monitor",
)

log = logging.getLogger("ju_weekly")


# ----------------------------------------------------------------------------
# PDF PARSING
# ----------------------------------------------------------------------------
def _norm(s):
    return re.sub(r"[^a-z]", "", (s or "").lower())


def _sig(s):
    """Letter-multiset signature. Rotated table headers come out of pdfplumber with
    scrambled character order, but the letters are intact, so compare sorted letters."""
    return "".join(sorted(_norm((s or "").replace("\n", ""))))


def _find_col(headers, variants):
    sigs = {_sig(v) for v in variants}
    for i, h in enumerate(headers):
        if _sig(h) in sigs:
            return i
    return None


def _page_index(pages, pattern):
    for i, p in enumerate(pages):
        if re.search(pattern, p.extract_text() or ""):
            return i
    return None


def _cell_int(v, blank_ok=False):
    v = (v or "").strip()
    if v == "":
        if blank_ok:
            return ""
        return 0
    return int(float(v))


def _cell_float(v):
    v = (v or "").strip()
    return "" if v == "" else float(v)


def week_from_pdf(path):
    with pdfplumber.open(path) as pdf:
        text = pdf.pages[0].extract_text() or ""
    m = re.search(r"Week\s+(\d+)\s*\(", text)
    return int(m.group(1)) if m else None


def parse_pdf(path, week):
    """Return dict with keys light, gravid, table2, table5 (lists of dicts)."""
    out = {"light": [], "gravid": [], "table2": [], "table5": []}
    with pdfplumber.open(path) as pdf:
        pages = pdf.pages

        # ---- Table 1 (Light / Moshar CO2 trap) ----
        i1 = _page_index(pages, r"Table 1\.")
        if i1 is None:
            raise RuntimeError("Table 1 not found")
        t = pages[i1].extract_tables()[0]
        h = t[0]
        zc = _find_col(h, ["Zone"])
        ac = _find_col(h, ["Ae.aegypti"])
        bc = _find_col(h, ["Ae.albopictus"])  # may legitimately be absent
        cc = _find_col(h, ["Cx.quinquefasciatus"])
        if None in (zc, ac, cc):
            raise RuntimeError(f"Table 1 header not recognised: {h}")
        for r in t[1:]:
            z = (r[zc] or "").strip()
            if re.fullmatch(r"[1-5]", z):
                out["light"].append({
                    "Zone": int(z),
                    "Ae.aegypti": _cell_int(r[ac]),
                    "Ae.albopictus": _cell_int(r[bc]) if bc is not None else 0,
                    "Cx.quinquefasciatus": _cell_int(r[cc]),
                    "type": "Light",
                    "week_ju": week,
                })

        # ---- Table 2 (Aedes X smart trap larvae) ----
        i2 = _page_index(pages, r"Table 2\.")
        if i2 is None:
            raise RuntimeError("Table 2 not found")
        t = pages[i2].extract_tables()[0]
        h = t[0]
        zc, ac, bc = (_find_col(h, ["Zone"]), _find_col(h, ["Ae.aegypti"]),
                      _find_col(h, ["Ae.albopictus"]))
        if None in (zc, ac, bc):
            raise RuntimeError(f"Table 2 header not recognised: {h}")
        for r in t[1:]:
            z = (r[zc] or "").strip()
            if re.fullmatch(r"[1-5]", z):
                out["table2"].append({
                    "zone": int(z),
                    "Ae.aegypti": _cell_int(r[ac]),
                    "Ae.albopictus": _cell_int(r[bc]),
                    "week_ju": week,
                })

        # ---- Table 5 (houses / containers / BI CI HI) ----
        i5 = _page_index(pages, r"Table 5")
        if i5 is None:
            raise RuntimeError("Table 5 not found")
        t = pages[i5].extract_tables()[0]
        h = t[0]
        cols = {
            "zone": _find_col(h, ["Zone"]),
            "Total_House": _find_col(h, ["TotalHouse"]),
            "Positive_House": _find_col(h, ["PositiveHouse"]),
            "Total_Wet_container": _find_col(h, ["TotalWetcontainer"]),
            "Positive_Wet_Container": _find_col(h, ["PositiveWetContainer"]),
            "BI": _find_col(h, ["BI"]),
            "CI": _find_col(h, ["CI"]),
            "HI": _find_col(h, ["HI"]),
        }
        if None in cols.values():
            raise RuntimeError(f"Table 5 header not recognised: {h}")
        for r in t[1:]:
            z = (r[cols["zone"]] or "").strip()
            if re.fullmatch(r"[1-5]", z):
                out["table5"].append({
                    "zone": int(z),
                    "Total_House": _cell_int(r[cols["Total_House"]], True),
                    "Positive_House": _cell_int(r[cols["Positive_House"]], True),
                    "Total_Wet_container": _cell_int(r[cols["Total_Wet_container"]], True),
                    "Positive_Wet_Container": _cell_int(r[cols["Positive_Wet_Container"]], True),
                    "BI": _cell_float(r[cols["BI"]]),
                    "CI": _cell_float(r[cols["CI"]]),
                    "HI": _cell_float(r[cols["HI"]]),
                    "week_ju": week,
                })

        # ---- Table 3 (Gravid trap) - only if it IS the gravid table ----
        i3 = _page_index(pages, r"Table 3\.")
        if i3 is not None:
            text = pages[i3].extract_text() or ""
            caption = text[text.find("Table 3."):][:220].lower()
            if "gravid" in caption:
                out["gravid"] = _parse_gravid(path, i3 + 1, week)
                if len(out["gravid"]) != 5:
                    raise RuntimeError(f"Gravid table: expected 5 zones, got {len(out['gravid'])}")
            else:
                log.warning("Week %s: Table 3 is not the Gravid-trap table (%s...) - no Gravid rows",
                            week, caption[:60].replace("\n", " "))
        else:
            log.warning("Week %s: Table 3 not found - no Gravid rows", week)

    for k, v in (("light", out["light"]), ("table2", out["table2"]), ("table5", out["table5"])):
        if len(v) != 5:
            raise RuntimeError(f"{k}: expected 5 zones, got {len(v)}")
    return out


def _parse_gravid(path, page_no, week):
    """pdfplumber merges this table's columns, so read it from pdftotext -layout."""
    txt = subprocess.run(
        ["pdftotext", "-layout", "-f", str(page_no), "-l", str(page_no), path, "-"],
        capture_output=True, text=True, check=True,
    ).stdout
    sec = txt[txt.find("Table 3."):]
    rows = []
    for line in sec.split("\n"):
        m = re.match(r"^\s*([1-5])\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s*$", line)
        if m:
            z, n, a, b, c = map(int, m.groups())
            rows.append({"Zone": z, "Ae.aegypti": a, "Ae.albopictus": b,
                         "Cx.quinquefasciatus": c, "type": "Gravid", "week_ju": week})
            continue
        # a zone reported as a single "0" (seen in week 98) -> all zeros
        m0 = re.match(r"^\s*([1-5])\s+0\s*$", line)
        if m0:
            rows.append({"Zone": int(m0.group(1)), "Ae.aegypti": 0, "Ae.albopictus": 0,
                         "Cx.quinquefasciatus": 0, "type": "Gravid", "week_ju": week})
    return rows


# ----------------------------------------------------------------------------
# DISCOVERY (the reports page is a JavaScript app -> headless Chromium)
# ----------------------------------------------------------------------------
def listing_url(page_no):
    filters = json.dumps({"reports_type": REPORTS_TYPE_ID}, separators=(",", ":"))
    return (f"{BASE_URL}/pages/reports?filters={quote(filters, safe='')}"
            f"&page={page_no}&page_size={PAGE_SIZE}&order=desc&order_by=title")


def _walk_strings(obj):
    if isinstance(obj, str):
        yield obj
    elif isinstance(obj, dict):
        for v in obj.values():
            yield from _walk_strings(v)
    elif isinstance(obj, list):
        for v in obj:
            yield from _walk_strings(v)


def _week_from_text(s):
    m = re.search(r"week(?:%20|[\s_\-])*(\d{2,3})", s, flags=re.I)
    return int(m.group(1)) if m else None


def discover_pdf_urls():
    """Return {url: week_or_None} for every PDF link found on the listing pages."""
    from playwright.sync_api import sync_playwright

    found = {}
    with sync_playwright() as p:
        browser = p.chromium.launch()
        ctx = browser.new_context(user_agent=USER_AGENT)
        page = ctx.new_page()

        json_payloads = []

        def on_response(resp):
            try:
                if "json" in (resp.headers.get("content-type") or ""):
                    json_payloads.append(resp.json())
            except Exception:
                pass

        page.on("response", on_response)

        for page_no in range(1, MAX_LIST_PAGES + 1):
            json_payloads.clear()
            url = listing_url(page_no)
            log.info("Loading listing page %s", page_no)
            page.goto(url, wait_until="networkidle", timeout=90_000)
            page.wait_for_timeout(3000)

            before = len(found)
            # 1) plain <a href=...pdf> links
            anchors = page.eval_on_selector_all(
                "a", "els => els.map(e => ({href: e.href, text: e.innerText}))")
            for a in anchors:
                href = a.get("href") or ""
                if re.search(r"\.pdf(\?|#|$)", href, flags=re.I):
                    found[href] = _week_from_text(href) or _week_from_text(a.get("text") or "")
            # 2) any PDF path inside the JSON the page itself fetched
            for payload in json_payloads:
                for s in _walk_strings(payload):
                    if re.search(r"\.pdf(\?|#|$)", s, flags=re.I):
                        full = urljoin(BASE_URL + "/", s)
                        found.setdefault(full, _week_from_text(s))
            if len(found) == before:   # nothing new on this page -> done
                break
        browser.close()

    log.info("Discovered %d PDF link(s)", len(found))
    return found


def download(url, dest):
    r = requests.get(url, headers={"User-Agent": USER_AGENT}, timeout=90)
    r.raise_for_status()
    if not r.content.startswith(b"%PDF"):
        raise RuntimeError(f"Not a PDF: {url}")
    with open(dest, "wb") as f:
        f.write(r.content)


# ----------------------------------------------------------------------------
# GOOGLE SHEETS
# ----------------------------------------------------------------------------
def open_sheet():
    import gspread
    from google.oauth2.service_account import Credentials

    raw = os.environ["GCP_SERVICE_ACCOUNT_JSON"]
    info = json.loads(raw)
    creds = Credentials.from_service_account_info(
        info, scopes=["https://www.googleapis.com/auth/spreadsheets"])
    return gspread.authorize(creds).open_by_key(SHEET_ID)


def get_tab(sh, name):
    import gspread
    try:
        return sh.worksheet(name)
    except gspread.WorksheetNotFound:
        have = [w.title for w in sh.worksheets()]
        raise SystemExit(f"Tab '{name}' not found. Tabs in the sheet: {have}. "
                         f"Set TAB_LIGHT_GRAVID / TAB_TABLE2 / TAB_TABLE5 accordingly.")


_ALIASES = {"aeaegypty": "aeaegypti"}   # header spelling variants seen in the sheet


def _key(s):
    k = re.sub(r"[^a-z0-9]", "", str(s).lower())
    return _ALIASES.get(k, k)


class Tab:
    """A worksheet whose header row decides column order."""

    def __init__(self, ws, needed_keys):
        self.ws = ws
        values = ws.get_all_values()
        if not values:
            raise SystemExit(f"Tab '{ws.title}' is empty - put the header row in row 1.")
        self.header = values[0]
        self.hkeys = [_key(h) for h in self.header]
        missing = [k for k in needed_keys if _key(k) not in self.hkeys]
        if missing:
            raise SystemExit(f"Tab '{ws.title}' has no column for {missing}. Header is {self.header}")
        self.week_i = self.hkeys.index(_key("week_ju"))
        self.type_i = self.hkeys.index(_key("type")) if _key("type") in self.hkeys else None
        self.seen = set()
        for r in values[1:]:
            if len(r) > self.week_i and str(r[self.week_i]).strip():
                try:
                    wk = int(float(r[self.week_i]))
                except ValueError:
                    continue
                typ = r[self.type_i].strip() if self.type_i is not None and len(r) > self.type_i else ""
                self.seen.add((wk, typ))

    def has(self, week, typ=""):
        return (week, typ) in self.seen

    def append(self, rows, typ=""):
        out = []
        for d in rows:
            by_key = {_key(k): v for k, v in d.items()}
            out.append([by_key.get(hk, "") for hk in self.hkeys])
        if out:
            self.ws.append_rows(out, value_input_option="USER_ENTERED")
        if rows:
            self.seen.add((rows[0]["week_ju"], typ))


# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--pdf", help="parse one local PDF and print the rows (no sheet access)")
    ap.add_argument("--dry-run", action="store_true", help="do everything except write to the sheet")
    args = ap.parse_args()
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")

    if args.pdf:
        wk = week_from_pdf(args.pdf)
        res = parse_pdf(args.pdf, wk)
        print(f"week {wk}")
        for k, v in res.items():
            print(f"-- {k}: {len(v)} rows")
            for r in v:
                print("  ", r)
        return 0

    sh = None if args.dry_run and "GCP_SERVICE_ACCOUNT_JSON" not in os.environ else open_sheet()
    tabs = None
    if sh is not None:
        tabs = {
            "lg": Tab(get_tab(sh, TAB_LIGHT_GRAVID),
                      ["Zone", "Ae.aegypti", "Ae.albopictus", "Cx.quinquefasciatus", "type", "week_ju"]),
            "t2": Tab(get_tab(sh, TAB_TABLE2), ["zone", "Ae.aegypti", "Ae.albopictus", "week_ju"]),
            "t5": Tab(get_tab(sh, TAB_TABLE5),
                      ["zone", "Total_House", "Positive_House", "Total_Wet_container",
                       "Positive_Wet_Container", "BI", "CI", "HI", "week_ju"]),
        }

    links = discover_pdf_urls()
    if not links:
        log.error("No PDF links found - the page structure may have changed.")
        return 1

    # resolve week numbers; PDFs whose link carries no week number are checked by content
    todo = {}
    tmp = tempfile.mkdtemp()
    for url, wk in links.items():
        path = None
        if wk is None:
            path = os.path.join(tmp, f"probe_{abs(hash(url))}.pdf")
            try:
                download(url, path)
                wk = week_from_pdf(path)
            except Exception as e:
                log.warning("Skipping %s (%s)", url, e)
                continue
        if wk is not None and wk > LAST_WEEK_DONE and wk not in todo:
            todo[wk] = (url, path)

    if not todo:
        log.info("Nothing new after week %s.", LAST_WEEK_DONE)
        return 0

    failures = 0
    for wk in sorted(todo):
        url, path = todo[wk]
        try:
            if tabs and all([tabs["lg"].has(wk, "Light"), tabs["t2"].has(wk), tabs["t5"].has(wk)]):
                log.info("Week %s already in the sheet - skipped", wk)
                continue
            if path is None:
                path = os.path.join(tmp, f"week_{wk}.pdf")
                download(url, path)
            pdf_week = week_from_pdf(path)
            if pdf_week not in (None, wk):
                raise RuntimeError(f"link says week {wk} but PDF says week {pdf_week}")
            res = parse_pdf(path, wk)
            log.info("Week %s parsed: light=%d gravid=%d t2=%d t5=%d", wk, len(res["light"]),
                     len(res["gravid"]), len(res["table2"]), len(res["table5"]))
            if args.dry_run or not tabs:
                log.info("dry-run: not writing week %s", wk)
                continue
            if not tabs["lg"].has(wk, "Light"):
                tabs["lg"].append(res["light"], "Light")
            if res["gravid"] and not tabs["lg"].has(wk, "Gravid"):
                tabs["lg"].append(res["gravid"], "Gravid")
            if not tabs["t2"].has(wk):
                tabs["t2"].append(res["table2"])
            if not tabs["t5"].has(wk):
                tabs["t5"].append(res["table5"])
            log.info("Week %s written", wk)
            time.sleep(1)
        except Exception as e:
            failures += 1
            log.exception("Week %s failed: %s", wk, e)

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())

"""Drive the Library panel in a jamovi container (see panel_test.py for usage)."""
import os, sys, json, zipfile
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "..", "jamovi-automation"))
from playwright.sync_api import sync_playwright
import jamovi_capture as jc

OUT = sys.argv[1] if len(sys.argv) > 1 else "."
XLSX = sys.argv[2] if len(sys.argv) > 2 else None
errors, fails = [], []

def check(cond, msg):
    print(("ok   " if cond else "FAIL ") + msg)
    if not cond: fails.append(msg)

def results_text(page):
    return "\n".join(f.locator("body").inner_text(timeout=3000) for f in page.frames if jc.HOST_C in f.url)

with sync_playwright() as p:
    browser = p.chromium.launch()
    ctx = browser.new_context(viewport={"width": 1440, "height": 1000}, accept_downloads=True)
    page = ctx.new_page()
    page.on("pageerror", lambda e: errors.append(str(e)))
    jc.connect(page)
    fr = jc.run_analysis(page, "Simulate", "Library", exact=False)
    page.wait_for_timeout(5000)
    txt = results_text(page)
    check("No problems found" in txt, "built-in library checks clean")
    check("Coronary heart disease incidence" in txt, "scenarios listed")
    sel = fr.locator("select.studysim-pick")
    check(sel.count() == 1, "scenario picker present")
    sel.first.select_option("bioequivalence")
    page.wait_for_timeout(3000)
    check("formulation" in results_text(page), "details follow the picker")
    page.screenshot(path=os.path.join(OUT, "11-library.png"))
    with page.expect_download(timeout=60000) as dl:
        fr.get_by_text("Export library", exact=True).click()
    path = os.path.join(OUT, dl.value.suggested_filename); dl.value.save_as(path)
    check(path.endswith("studysim-library.xlsx") and zipfile.is_zipfile(path), "library exported as xlsx: " + os.path.basename(path))
    with page.expect_download(timeout=60000) as dl:
        fr.get_by_text("Excel template", exact=True).click()
    path = os.path.join(OUT, dl.value.suggested_filename); dl.value.save_as(path)
    check(os.path.basename(path) == "studysim-template.xlsx", "template downloaded")
    if XLSX:
        with page.expect_file_chooser(timeout=10000) as fc:
            fr.locator("button.studysim-browse").click()
        fc.value.set_files(XLSX)
        page.wait_for_timeout(5000)
        txt = results_text(page)
        check("custom" in txt and "takes 2 values" in txt, "custom Excel library read and checked")
        check(fr.locator("select.studysim-pick").count() == 0, "non-JSON library: scenario typed, no picker")
        page.screenshot(path=os.path.join(OUT, "12-custom-xlsx.png"))
    browser.close()

check(not errors, "no page errors " + json.dumps(errors[:5]))
sys.exit(1 if fails else 0)

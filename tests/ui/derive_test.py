"""Drive the Derive scenario panel on a CSV in the container's Documents (see panel_test.py)."""
import os, sys, json
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "..", "jamovi-automation"))
from playwright.sync_api import sync_playwright
import jamovi_capture as jc

OUT = sys.argv[1] if len(sys.argv) > 1 else "."
CSV = sys.argv[2] if len(sys.argv) > 2 else "chd_sim.csv"
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
    jc.open_this_pc_file(page, CSV)
    fr = jc.run_analysis(page, "Simulate", "Derive scenario", exact=False)
    page.wait_for_timeout(2000)
    jc.add_variable(fr, "chd", 0)
    jc.add_variable(fr, "time", 1)
    for v in ["age", "sex", "smoking", "sbp", "coffee"]:
        jc.add_variable(fr, v, 2)
    for i in range(40):
        page.wait_for_timeout(1000)
        txt = results_text(page)
        if "Re-estimated" in txt and "should be close" in txt: break
    check("event" in txt, "event outcome detected")
    check("read as numbers although their type is nominal: age, coffee" in txt, "integer CSV columns read as numbers")
    check("per 1" in txt and "41 vs 40" not in txt, "age is a continuous effect")
    check("Latent correlations" in txt and "derivation:" in txt, "correlations and YAML shown")
    check("should be close" in txt, "round-trip check filled")
    page.screenshot(path=os.path.join(OUT, "31-derive.png"), full_page=True)
    with page.expect_download(timeout=60000) as dl:
        fr.get_by_text("Export scenario", exact=True).click()
    path = os.path.join(OUT, dl.value.suggested_filename); dl.value.save_as(path)
    head = open(path, encoding="utf-8").read()
    check(os.path.basename(path) == "derived_scenario.yaml" and "outcomes:" in head, "scenario exported: " + os.path.basename(path))
    browser.close()

check(not errors, "no page errors " + json.dumps(errors[:5]))
sys.exit(1 if fails else 0)

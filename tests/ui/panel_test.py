"""Drive the Simulated study panel in a jamovi container (jamovi-automation).

    JAMOVI_PORT=41737 JAMOVI_CONTAINER=jamovi-studysim \
        sh ../jamovi-automation/run.sh tests/ui/panel_test.py <outdir>

Fails (exit 1) on a page error or a broken expectation; writes screenshots.
"""
import os, sys, json, time
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "..", "jamovi-automation"))
from playwright.sync_api import sync_playwright
import jamovi_capture as jc

OUT = sys.argv[1] if len(sys.argv) > 1 else "."
HERE = os.path.dirname(os.path.abspath(__file__))
errors, fails = [], []

def check(cond, msg):
    print(("ok   " if cond else "FAIL ") + msg)
    if not cond: fails.append(msg)

def results_text(page):
    out = []
    for f in page.frames:
        if jc.HOST_C in f.url:
            try: out.append(f.locator("body").inner_text(timeout=2000))
            except Exception: pass
    return "\n".join(out)

with sync_playwright() as p:
    browser = p.chromium.launch()
    ctx = browser.new_context(viewport={"width": 1440, "height": 1000}, accept_downloads=True)
    page = ctx.new_page()
    page.on("pageerror", lambda e: errors.append(str(e)))
    jc.connect(page)
    fr = jc.run_analysis(page, "Simulate", "Simulated study", exact=False)
    page.wait_for_timeout(4000)
    sels = fr.locator("select.studysim-pick")
    check(sels.count() == 2, "two injected pickers")
    scen = sels.nth(0).locator("option").all_inner_texts()
    check(len(scen) >= 6, "scenario picker lists the built-in library (%d)" % len(scen))
    design = fr.locator("select.silky-option-combo-input").first
    vis = design.evaluate("s => Array.from(s.options).filter(o => !o.hidden).map(o => o.text)")
    check(vis == ["Case-control", "Matched case-control", "Cohort"], "breast cancer designs: %s" % vis)
    page.screenshot(path=os.path.join(OUT, "01-default.png"))
    txt = results_text(page)
    check("cases and" in txt, "summary shows cases and controls")

    sels.nth(0).select_option("chd_incidence")
    page.wait_for_timeout(3000)
    oc = sels.nth(1).locator("option").all_inner_texts()
    check(oc == ["Coronary heart disease event"], "chd outcome: %s" % oc)
    design.select_option(label="Cohort")
    page.wait_for_timeout(3000)
    target = fr.locator(".silky-list-item")
    print("list items:", target.all_inner_texts())
    jc.expand_section(fr, "Output")
    jc.check_option(fr, "Answer key")
    page.wait_for_timeout(6000)
    txt = results_text(page)
    check("what this design should estimate" in txt, "answer key shown")
    check("coffee" in txt, "coffee in the key")
    page.screenshot(path=os.path.join(OUT, "02-chd-cohort-key.png"))

    n_pages = len(ctx.pages)
    fr.get_by_text("Open dataset", exact=True).click()
    for i in range(60):
        page.wait_for_timeout(1000)
        if len(ctx.pages) > n_pages: break
    check(len(ctx.pages) > n_pages, "Open dataset opened a new window")
    if len(ctx.pages) > n_pages:
        np_ = ctx.pages[-1]
        np_.wait_for_timeout(12000)
        np_.screenshot(path=os.path.join(OUT, "03-dataset.png"))
        np_.close()
    with page.expect_download(timeout=60000) as dl:
        fr.get_by_text("Export CSV", exact=True).click()
    path = os.path.join(OUT, "export.csv")
    dl.value.save_as(path)
    head = open(path, encoding="utf-8").readline().strip()
    check(head.startswith('"id","time","chd"'), "CSV header: " + head[:60])

    with page.expect_file_chooser(timeout=10000) as fc:
        fr.locator("button.studysim-browse").click()
    fc.value.set_files(os.path.join(HERE, "custom-library.json"))
    page.wait_for_timeout(4000)
    scen2 = sels.nth(0).locator("option").all_inner_texts()
    check(any("custom" in s.lower() for s in scen2), "custom scenario in the picker")
    sels.nth(0).select_option("bioeq_custom")
    page.wait_for_timeout(4000)
    vis = design.evaluate("s => Array.from(s.options).filter(o => !o.hidden).map(o => o.text)")
    check(vis == ["Randomised trial", "2x2 crossover trial"], "custom designs: %s" % vis)
    page.wait_for_timeout(3000)
    txt = results_text(page)
    check("formulation" in txt, "custom scenario simulated")
    page.screenshot(path=os.path.join(OUT, "04-custom.png"))
    browser.close()

check(not errors, "no page errors " + json.dumps(errors[:5]))
sys.exit(1 if fails else 0)

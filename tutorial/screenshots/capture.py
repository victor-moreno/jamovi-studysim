"""Screenshots for the tutorial, from jamovi in Docker (jamovi-automation).

    JAMOVI_CONTAINER=jamovi-studysim \
        sh ../jamovi-automation/run.sh tutorial/screenshots/capture.py

Needs the module installed in the container and tutorial/screenshots/chd_sim.csv
copied into its Documents (the script does the copy).
"""
import os, sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "..", "jamovi-automation"))
from playwright.sync_api import sync_playwright
import jamovi_capture as jc

def out(name): return os.path.join(HERE, name)

def result_item(page, text):
    """The results element (table, image, html) whose title is `text`."""
    for f in page.frames:
        if jc.HOST_C not in f.url: continue
        loc = f.get_by_text(text, exact=True)
        for i in range(loc.count()):
            el = loc.nth(i)
            if el.is_visible():
                item = el.locator("xpath=ancestor::*[contains(@class,'jmv-results-item')][1]")
                return item if item.count() else el
    return None

def shot_item(page, text, name):
    item = result_item(page, text)
    if item is None:
        print("missing:", text); return
    item.scroll_into_view_if_needed(); page.wait_for_timeout(800)
    item.screenshot(path=out(name))

def wait_text(page, text, secs=40):
    for _ in range(secs):
        t = "\n".join(f.locator("body").inner_text() for f in page.frames if jc.HOST_C in f.url)
        if text in t: return True
        page.wait_for_timeout(1000)
    return False

with sync_playwright() as p:
    browser = p.chromium.launch()
    ctx = browser.new_context(viewport={"width": 1440, "height": 1000}, accept_downloads=True, device_scale_factor=2)
    page = ctx.new_page()
    jc.connect(page)

    # Simulated study: CHD cohort with a few measured variables
    fr = jc.run_analysis(page, "Simulate", "Simulated study", exact=False)
    wait_text(page, "cases and")
    fr.locator("select.studysim-pick").nth(0).select_option("chd_incidence"); page.wait_for_timeout(3000)
    fr.locator("select.silky-option-combo-input").first.select_option(label="Cohort"); page.wait_for_timeout(3000)
    ids = [t.strip() for t in fr.locator(".list-item-cell .silky-control-label").all_inner_texts()][0::2]
    boxes = fr.locator(".list-item-cell input[type=checkbox]")
    forms = fr.locator(".list-item-cell select")
    for v in ["age", "sex", "smoking", "sbp", "coffee"]:
        boxes.nth(ids.index(v)).check(force=True); page.wait_for_timeout(500)
    forms.nth(ids.index("sbp")).select_option(label="quartiles"); page.wait_for_timeout(3000)
    wait_text(page, "Q4: >")          # results of this scenario, not the previous one
    page.wait_for_timeout(3000)
    fr.locator(".list-item-cell").first.scroll_into_view_if_needed()
    fr.evaluate("() => document.querySelectorAll('.list-item-cell')[0].closest('[class*=list]').scrollTop = 0")
    page.wait_for_timeout(500)
    page.screenshot(path=out("s01-simulated-study.png"))
    page.screenshot(path=out("s02-panel.png"), clip=jc.OPTIONS_PANEL_CLIP)
    shot_item(page, "Data dictionary", "s03-dictionary.png")
    shot_item(page, "Descriptives", "s04-descriptives.png")

    jc.expand_section(fr, "Output")
    jc.check_option(fr, "Causal diagram (DAG)")
    jc.check_option(fr, "Effect sizes on the diagram")
    jc.check_option(fr, "Answer key")
    wait_text(page, "should be close") or wait_text(page, "Expected values")
    page.wait_for_timeout(4000)
    shot_item(page, "Causal diagram", "s05-dag.png")
    shot_item(page, "Answer key: true model", "s06-key-true.png")
    shot_item(page, "Answer key: what this design should estimate", "s07-key-expected.png")

    jc.expand_section(fr, "Design options")
    page.wait_for_timeout(800)
    jc.screenshot_section(page, fr, "Design options", out("s09-design-options.png"), stop_label="Output")

    n = len(ctx.pages)
    fr.get_by_text("Open dataset", exact=True).click()
    for _ in range(60):
        page.wait_for_timeout(1000)
        if len(ctx.pages) > n: break
    if len(ctx.pages) > n:
        dp = ctx.pages[-1]; dp.set_viewport_size({"width": 1440, "height": 700}); dp.wait_for_timeout(12000)
        dp.screenshot(path=out("s10-dataset.png")); dp.close()

    # Library
    fr = jc.run_analysis(page, "Simulate", "Library", exact=False)
    wait_text(page, "No problems found")
    fr.locator("select.studysim-pick").first.select_option("breast_cancer_risk"); page.wait_for_timeout(4000)
    page.screenshot(path=out("s11-library.png"))

    # Derive scenario on a CSV
    jc.put_file(out("chd_sim.csv"))
    page.goto(jc.JAMOVI_URL); page.wait_for_timeout(6000)
    jc.open_this_pc_file(page, "chd_sim.csv")
    fr = jc.run_analysis(page, "Simulate", "Derive scenario", exact=False)
    page.wait_for_timeout(1500)
    jc.add_variable(fr, "chd", 0); jc.add_variable(fr, "time", 1)
    for v in ["age", "sex", "smoking", "sbp", "coffee"]:
        jc.add_variable(fr, v, 2)
    wait_text(page, "should be close", 60)
    page.wait_for_timeout(2000)
    page.screenshot(path=out("s12-derive.png"))
    shot_item(page, "Conditional effects on the outcome", "s13-derive-effects.png")
    browser.close()
print("done")

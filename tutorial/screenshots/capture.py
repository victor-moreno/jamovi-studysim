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

def widen_data(dp):
    """Drag the splitter between the spreadsheet and the (empty) results panel
    to the right edge, so the dataset shows all its columns."""
    vw = dp.viewport_size["width"]
    sp = dp.locator(".silky-splitpanel-splitter:not(.hidden-panel)").first
    box = sp.bounding_box() if sp.count() else None
    x0 = box["x"] + box["width"] / 2 if box else 0.42 * vw
    y0 = box["y"] + min(box["height"] / 2, 300) if box else 400
    dp.mouse.move(x0, y0); dp.mouse.down()
    dp.mouse.move(vw - 8, y0, steps=12); dp.mouse.up()
    dp.wait_for_timeout(1500)

with sync_playwright() as p:
    browser = p.chromium.launch()
    ctx = browser.new_context(viewport={"width": 1440, "height": 1000}, accept_downloads=True, device_scale_factor=2)
    page = ctx.new_page()
    jc.connect(page)

    # Simulated study: CHD cohort with a few measured variables
    fr = jc.run_analysis(page, "Simulate", "Simulated study", exact=False)
    wait_text(page, "cases and")
    fr.locator("select.studysim-pick").nth(0).select_option("chd_incidence"); page.wait_for_timeout(3000)
    # the first combo is the design filter, the second the design
    design = fr.locator("select.silky-option-combo-input").nth(1)
    design.select_option(label="Cohort"); page.wait_for_timeout(3000)
    ids = [t.strip() for t in fr.locator(".list-item-cell .silky-control-label").all_inner_texts()][0::2]
    boxes = fr.locator(".list-item-cell input[type=checkbox]")   # Measured and Exposure ticks alternate
    forms = fr.locator(".list-item-cell select")
    # every variable is measured by default: untick those left out
    for v in ["education", "physical_activity", "bmi", "cigarettes", "total_chol", "hdl", "diabetes"]:
        boxes.nth(2 * ids.index(v)).uncheck(force=True); page.wait_for_timeout(500)
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
    jc.check_option(fr, "Effect sizes on the diagram")   # off by default since v0.5
    jc.check_option(fr, "Answer key")
    wait_text(page, "should be close") or wait_text(page, "Expected values")
    page.wait_for_timeout(6000)
    shot_item(page, "Causal diagram", "s05-dag.png")
    # the key tables are wide: a wider window for them
    page.set_viewport_size({"width": 2000, "height": 1000}); page.wait_for_timeout(3000)
    shot_item(page, "Answer key: true model", "s06-key-true.png")
    shot_item(page, "Answer key: what this design should estimate", "s07-key-expected.png")
    page.set_viewport_size({"width": 1440, "height": 1000}); page.wait_for_timeout(2000)

    jc.expand_section(fr, "Design options")
    page.wait_for_timeout(800)
    jc.screenshot_section(page, fr, "Design options", out("s09-design-options.png"), stop_label="Biases")
    jc.expand_section(fr, "Biases")
    page.wait_for_timeout(800)
    jc.screenshot_section(page, fr, "Biases", out("s18-biases.png"), stop_label="Realism")
    jc.expand_section(fr, "Realism")
    page.wait_for_timeout(800)
    jc.screenshot_section(page, fr, "Realism", out("s14-realism.png"), stop_label="Output")

    n = len(ctx.pages)
    fr.get_by_text("Open dataset", exact=True).click()
    for _ in range(60):
        page.wait_for_timeout(1000)
        if len(ctx.pages) > n: break
    if len(ctx.pages) > n:
        dp = ctx.pages[-1]; dp.set_viewport_size({"width": 1440, "height": 700}); dp.wait_for_timeout(12000)
        widen_data(dp)
        dp.screenshot(path=out("s10-dataset.png")); dp.close()

    # Several outcomes: SPRINT, with the other outcomes of the same people
    # (a fresh instance, so the results hold only this analysis)
    page.goto(jc.JAMOVI_URL); page.wait_for_timeout(6000)
    fr = jc.run_analysis(page, "Simulate", "Simulated study", exact=False)
    wait_text(page, "cases and")
    fr.locator("select.studysim-pick").nth(0).select_option("sprint_blood_pressure"); page.wait_for_timeout(4000)
    # the other outcomes are the first rows of the Variables list: tick them
    page.wait_for_timeout(2000)
    names = [t.strip() for t in fr.locator(".list-item-cell .silky-control-label").all_inner_texts()][0::2]
    boxes = fr.locator(".list-item-cell input[type=checkbox]")
    for i in range(4):
        boxes.nth(2 * i).check(force=True); page.wait_for_timeout(800)
    jc.expand_section(fr, "Output")
    jc.check_option(fr, "Answer key")
    wait_text(page, "Answer key: what this design should estimate", 60)
    page.wait_for_timeout(6000)
    # the table is wide: a wider window for this one
    page.set_viewport_size({"width": 2300, "height": 1000}); page.wait_for_timeout(3000)
    shot_item(page, "Answer key: what this design should estimate", "s16-several-outcomes.png")
    page.set_viewport_size({"width": 1440, "height": 1000})

    # A trial's absolute effects: indomethacin after ERCP
    page.goto(jc.JAMOVI_URL); page.wait_for_timeout(6000)
    fr = jc.run_analysis(page, "Simulate", "Simulated study", exact=False)
    wait_text(page, "cases and")
    fr.locator("select.studysim-pick").nth(0).select_option("indomethacin_ercp"); page.wait_for_timeout(4000)
    jc.expand_section(fr, "Output")
    jc.check_option(fr, "Answer key")
    wait_text(page, "Answer key: absolute effects in the trial", 60)
    page.wait_for_timeout(8000)
    shot_item(page, "Answer key: absolute effects in the trial", "s19-key-trial.png")

    # Compare designs: breast cancer, its default designs
    page.goto(jc.JAMOVI_URL); page.wait_for_timeout(6000)
    fr = jc.run_analysis(page, "Simulate", "Compare designs", exact=False)
    wait_text(page, "Estimates by design", 60)
    # four variables keep the table and the plot short
    page.wait_for_timeout(3000)
    ids = [t.strip() for t in fr.locator(".list-item-cell .silky-control-label").all_inner_texts()][0::2]
    boxes = fr.locator(".list-item-cell input[type=checkbox]")
    for v in ids:
        if v not in ("age", "menopause", "hrt", "family_hist"):
            boxes.nth(ids.index(v)).uncheck(force=True); page.wait_for_timeout(400)
    wait_text(page, "All designs sample the same superpopulation", 90)
    page.wait_for_timeout(8000)
    page.screenshot(path=out("s15-compare.png"))
    # the estimates plot: the last image already drawn (the replicates plot,
    # hidden without replicates, comes after it and is empty)
    for _ in range(60):
        drawn = [e for f in page.frames if jc.HOST_C in f.url for e in f.locator(".jmv-results-image-image").all()
                 if e.is_visible() and "url(" in (e.evaluate("e => e.style.backgroundImage") or "")]
        if len(drawn) >= 2: break
        page.wait_for_timeout(1000)
    if drawn:
        drawn[-1].scroll_into_view_if_needed(); page.wait_for_timeout(1500)
        drawn[-1].screenshot(path=out("s17-compare-plot.png"))
    else:
        print("missing: compare plot")

    # Replicate studies: indomethacin, randomised and cluster trials, 200 each
    page.goto(jc.JAMOVI_URL); page.wait_for_timeout(6000)
    fr = jc.run_analysis(page, "Simulate", "Compare designs", exact=False)
    wait_text(page, "Estimates by design", 60)
    fr.locator("select.studysim-pick").nth(0).select_option("indomethacin_ercp"); page.wait_for_timeout(5000)
    jc.fill_text(fr, "Replicate studies per design", "200")
    wait_text(page, "studies of 500 per design", 240)
    page.wait_for_timeout(8000)
    page.set_viewport_size({"width": 2100, "height": 1000}); page.wait_for_timeout(3000)
    shot_item(page, "Replicate studies", "s20-replicates.png")
    page.set_viewport_size({"width": 1440, "height": 1000})

    # Library
    page.goto(jc.JAMOVI_URL); page.wait_for_timeout(6000)
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

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
    for i in range(20):   # the first run after a restart loads R and calibrates
        txt = results_text(page)
        if "cases and" in txt: break
        page.wait_for_timeout(1000)
    check("cases and" in txt, "summary shows cases and controls")

    sels.nth(0).select_option("chd_incidence")
    page.wait_for_timeout(3000)
    oc = sels.nth(1).locator("option").all_inner_texts()
    check(oc == ["Coronary heart disease event"], "chd outcome: %s" % oc)
    design.select_option(label="Cohort")
    page.wait_for_timeout(3000)
    names = [t.strip() for t in fr.locator(".list-item-cell .silky-control-label").all_inner_texts()]
    check("coffee" in names and "age" in names, "checklist lists the variables: %s" % names[:6])
    boxes = fr.locator(".list-item-cell input[type=checkbox]")
    check(boxes.count() >= 12, "one checkbox per variable (%d)" % boxes.count())
    check(all(not boxes.nth(i).is_checked() for i in range(boxes.count())), "none measured at start")
    ids = ["age", "sex", "education", "physical_activity", "bmi", "smoking", "cigarettes", "sbp", "total_chol", "hdl", "diabetes", "coffee"]
    for v in ["age", "sex", "smoking", "coffee"]:
        boxes.nth(ids.index(v)).check(force=True)
        page.wait_for_timeout(700)
    boxes.nth(ids.index("coffee")).uncheck(force=True)
    page.wait_for_timeout(1500)
    labels = [t.strip() for t in fr.locator(".list-item-cell .silky-control-label").all_inner_texts()]
    check("years" in labels and "female/male" in labels and "never/former/current" in labels, "units/categories column")
    sel0 = fr.locator(".list-item-cell select")
    check(sel0.nth(ids.index("sex")).is_disabled() and not sel0.nth(ids.index("age")).is_disabled()
          and not sel0.nth(ids.index("coffee")).is_disabled(), "form only for numeric variables")
    forms = fr.locator(".list-item-cell select")
    cuts = fr.locator(".list-item-cell input[type=text]")
    check(forms.count() == len(ids) and cuts.count() == len(ids), "form and cut-point columns (%d, %d)" % (forms.count(), cuts.count()))
    for v in ["sbp", "bmi"]:
        boxes.nth(ids.index(v)).check(force=True); page.wait_for_timeout(600)
    forms.nth(ids.index("sbp")).select_option(label="quartiles"); page.wait_for_timeout(800)
    cuts.nth(ids.index("bmi")).fill("25, 30")
    boxes.nth(ids.index("coffee")).check(force=True); page.wait_for_timeout(600)
    cuts.nth(ids.index("coffee")).fill("0"); cuts.nth(ids.index("coffee")).press("Enter"); page.wait_for_timeout(600)
    forms.nth(ids.index("coffee")).select_option(label="cut points"); page.wait_for_timeout(800); cuts.nth(ids.index("bmi")).press("Enter"); page.wait_for_timeout(600)
    forms.nth(ids.index("bmi")).select_option(label="cut points"); page.wait_for_timeout(6000)
    t = results_text(page)
    check("Q4: >" in t and "G3: >30" in t and "G1: ≤0" in t and "0.000" not in t, "categorised variables in the descriptives, cut points as typed")
    boxes.nth(ids.index("coffee")).uncheck(force=True); page.wait_for_timeout(1500)
    page.screenshot(path=os.path.join(OUT, "09-categorised.png"))
    jc.expand_section(fr, "Output")
    jc.check_option(fr, "Causal diagram (DAG)")
    page.wait_for_timeout(8000)
    def dag_url():
        for f in page.frames:
            if jc.HOST_C in f.url:
                for e in f.locator(".jmv-results-image-image").all():
                    return e.evaluate("e => e.style.backgroundImage")
        return None
    def summary_text():
        return results_text(page).split("Data dictionary")[0]
    u0, s0 = dag_url(), summary_text()
    jc.check_option(fr, "Answer key")
    page.wait_for_timeout(8000)
    check(dag_url() == u0 and "url(" in (u0 or ""), "answer key does not redraw the DAG")
    check(summary_text() == s0, "answer key does not regenerate the study")
    txt = results_text(page)
    check("what this design should estimate" in txt, "answer key shown")
    import re
    bad = [txt[max(0, m.start() - 120):m.end() + 20] for m in re.finditer(r"NaN|\bnone\b", txt)]
    check(not bad, "null variables have a measure and a value " + json.dumps(bad[:2]))
    check("Descriptives" in txt and "mean (SD)" in txt, "descriptives table")
    imgs = [f.locator(".jmv-results-image-image") for f in page.frames if jc.HOST_C in f.url]
    imgs = [l.nth(i) for l in imgs for i in range(l.count())]
    drawn = [i for i in imgs if "url(" in (i.evaluate("e => e.style.backgroundImage") or "")]
    check(len(drawn) >= 1, "DAG image rendered")
    if drawn:
        drawn[0].scroll_into_view_if_needed(); page.wait_for_timeout(1500)
        drawn[0].screenshot(path=os.path.join(OUT, "05-dag.png"))
    est_cohort = "HR" in txt
    page.screenshot(path=os.path.join(OUT, "02-chd-cohort-key.png"), full_page=True)
    design.select_option(label="Case-control")
    page.wait_for_timeout(8000)
    txt2 = results_text(page)
    k = txt2[txt2.find("what this design should estimate"):]
    check(est_cohort and "OR" in k[:600] and "\tHR" not in k[:600], "answer key follows the design (HR -> OR)")
    design.select_option(label="Cohort")
    page.wait_for_timeout(6000)
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
    check(head == '"id","time","chd","age","sex","smoking","bmi","sbp"', "CSV header: " + head[:60])

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

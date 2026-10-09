# Validate a studysim scenario file and compare it with the data it was derived from.
#   Rscript check_scenario.R scenario.yaml
# Uses the installed studysim package, or its source in STUDYSIM_SRC (pkgload).
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("usage: Rscript check_scenario.R scenario.yaml")
src <- Sys.getenv("STUDYSIM_SRC")
if (nzchar(src)) pkgload::load_all(src, quiet = TRUE) else suppressPackageStartupMessages(library(studysim))
ns <- asNamespace("studysim")
get <- function(f) get0(f, envir = ns)

custom <- get("read_library_file")(args[1])
lib <- get("merge_libraries")(get("builtin_library")(), custom)
p <- get("check_library")(lib)
p <- p[p$scenario %in% names(custom$scenarios), ]
cat("== check:", if (nrow(p)) "" else "no problems", "\n")
if (nrow(p)) print(p, row.names = FALSE)
if (any(p$level == "error")) quit(status = 1)

for (id in names(custom$scenarios)) {
    raw <- custom$scenarios[[id]]
    sc <- get("load_scenario")(lib, id)
    get("use_i18n")(sc)
    ren <- unlist(raw$derivation$renamed)
    for (o in names(sc$outcomes)) {
        oc <- sc$outcomes[[o]]
        design <- if (oc$type == "event") "cohort" else "cross_sectional"
        if (!design %in% get("compatible_designs")(sc, o)) design <- get("compatible_designs")(sc, o)[1]
        opt <- get("design_defaults")(design, oc, sc)
        if (!is.null(opt$loss)) opt$loss <- 0
        ex <- get("expected_estimates")(sc, oc, design, opt, sc$order)
        cat(sprintf("\n== %s / %s: %s simulated with design %s\n", id, o, format(attr(ex, "n"), big.mark = ","), design))
        out <- data.frame(variable = ex$variable, contrast = ex$contrast, true = signif(ex$true, 3),
                          simulated = signif(ex$adjusted, 3), measure = ex$measure, data = NA_real_)
        for (e in raw$derivation$estimates) {
            v <- if (e$variable %in% names(ren)) ren[[e$variable]] else e$variable
            i <- which(out$variable == v & (out$contrast == e$contrast | grepl("^per", out$contrast) & grepl("^per", e$contrast)))
            if (length(i) == 1 && grepl("^per", e$contrast)) {
                per <- as.numeric(sub("^per ([0-9.eE+-]+).*$", "\\1", out$contrast[i]))
                out$data[i] <- signif(if (out$measure[i] == "beta") e$adjusted * per else e$adjusted^per, 3)
            } else if (length(i) == 1) out$data[i] <- e$adjusted
        }
        # categorical contrasts against a reference the AI changed: rebuild them
        # from the data's level-vs-old-reference estimates
        for (v in unique(out$variable[is.na(out$data) & grepl(" vs ", out$contrast)])) {
            r <- c()
            for (e in raw$derivation$estimates) {
                vv <- if (e$variable %in% names(ren)) ren[[e$variable]] else e$variable
                if (vv != v || !grepl(" vs ", e$contrast)) next
                lv <- strsplit(e$contrast, " vs ")[[1]]
                r[lv[2]] <- if (out$measure[out$variable == v][1] == "beta") 0 else 1
                r[lv[1]] <- e$adjusted
            }
            for (i in which(out$variable == v & is.na(out$data))) {
                lv <- strsplit(out$contrast[i], " vs ")[[1]]
                if (all(lv %in% names(r)))
                    out$data[i] <- signif(if (out$measure[i] == "beta") r[[lv[1]]] - r[[lv[2]]] else r[[lv[1]]] / r[[lv[2]]], 3)
            }
        }
        out$flag <- ifelse(!is.na(out$data) & abs(log(abs(out$simulated / out$data))) > log(1.15), "  <-- differs", "")
        print(out, row.names = FALSE)
    }
}

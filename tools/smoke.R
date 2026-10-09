# Smoke test, run by tools/install.sh right after installing the module into
# jamovi desktop and/or the docker container (module attached, jamovi's base
# library first on .libPaths()).
r <- studysim::simStudy(data = data.frame(), scenario = "antihypertensive_trial", outcome = "sbp",
                        design = "rct", n = 200, showKey = TRUE)
d <- r$dictionary$asDF
stopifnot(identical(d$name[1:3], c("id", "drug", "sbp")))
k <- r$keyExpected$asDF
stopifnot(abs(k$adjusted[1] + 9) < 1.5)
cat("   smoke test passed\n")

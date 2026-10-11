# Structure of real datasets that a copula cannot learn, found before Derive
# estimates anything:
# - codes for "unknown / not applicable" written as numbers (99, 999): an
#   all-nines value at the top of the range, far more frequent than the values
#   just below it, is read as missing;
# - structural values: a variable that does not apply in some level of another
#   one (age at first oral contraceptive use when never used) is missing, or
#   one constant, in those rows. The scenario writes it with only_if/otherwise,
#   its distribution comes from the rows where it applies, and the outcome model
#   gives the other rows the `otherwise` value (which is what the engine adds to
#   their linear predictor), so no rows are lost;
# - orders that the data never break (start age <= end age <= age): the engine
#   cannot keep them, so they are reported.

# list(df, codes): columns with a numeric code recoded to NA, and what was done
recode_codes <- function(df, cols) {
    codes <- list()
    for (v in cols) {
        x <- df[[v]]
        if (!is.numeric(x)) next
        y <- x[is.finite(x)]
        if (length(y) < 20) next
        top <- max(y)
        if (top < 9 || !grepl("^9+$", format(top, scientific = FALSE))) next
        n_top <- sum(y == top)
        below <- sort(unique(y[y < top]), decreasing = TRUE)[1:5]
        n_below <- max(vapply(below[!is.na(below)], function(b) sum(y == b), 0), 0)
        if (n_top >= 5 && n_top >= 0.02 * length(y) && n_top > 3 * n_below) {
            df[[v]][!is.na(x) & x == top] <- NA
            codes[[v]] <- list(value = top, n = n_top)
        }
    }
    list(df = df, codes = codes)
}

# For each variable x, the levels of another variable z in which x does not
# apply: missing (or one constant) in >= 95% of those rows, against < 50% in
# the other levels. Returns list(x = list(by, levels, value)); value NA when
# the rows are missing.
find_structural <- function(df, cols, min_rows = 10) {
    cat_cols <- cols[vapply(cols, function(v) { x <- df[[v]]; is.factor(x) || is.character(x) || is.logical(x) ||
                                                length(unique(stats::na.omit(x))) <= 5 }, TRUE)]
    out <- list()
    for (x in cols) {
        vx <- df[[x]]
        for (z in setdiff(cat_cols, x)) {
            vz <- as.character(df[[z]])
            lv <- sort(unique(stats::na.omit(vz)))
            if (length(lv) < 2) next
            ok <- !is.na(vz)
            # what x is in each level: share missing, and its most frequent value
            na_share <- vapply(lv, function(l) mean(is.na(vx[ok & vz == l])), 0)
            mode_of <- function(l) { t <- table(vx[ok & vz == l]); if (length(t)) names(t)[which.max(t)] else NA_character_ }
            const <- vapply(lv, mode_of, "")
            const_share <- vapply(lv, function(l) { y <- vx[ok & vz == l]; if (is.na(const[l])) 0 else mean(!is.na(y) & as.character(y) == const[l]) }, 0)
            size <- vapply(lv, function(l) sum(ok & vz == l), 0)
            # missing in the level, observed elsewhere
            s_na <- lv[na_share >= 0.95 & size >= min_rows]
            if (length(s_na) && length(s_na) < length(lv) && all(na_share[setdiff(lv, s_na)] < 0.5)) {
                out[[x]] <- list(by = z, levels = s_na, value = NA)
                break
            }
            # one constant in the level (and rare elsewhere); not for x itself categorical with 2 levels,
            # where "constant" would only restate a strong association
            if (length(unique(stats::na.omit(vx))) <= 2) next
            s_c <- lv[const_share >= 0.95 & size >= min_rows]
            if (length(s_c) && length(s_c) < length(lv) && length(unique(const[s_c])) == 1) {
                other <- ok & !vz %in% s_c & !is.na(vx)
                if (mean(as.character(vx[other]) == const[s_c[1]]) < 0.5) {
                    out[[x]] <- list(by = z, levels = s_c, value = const[s_c[1]])
                    break
                }
            }
        }
    }
    out
}

# the only_if text for a structural variable (ids and level labels of the scenario)
structural_rule <- function(s, ids, zlevels) {
    keep <- setdiff(zlevels, s$levels)
    q <- function(l) sprintf("\"%s\"", l)
    if (length(keep) == 1) sprintf("%s == %s", ids[[s$by]], q(keep))
    else if (length(s$levels) == 1) sprintf("%s != %s", ids[[s$by]], q(s$levels))
    else sprintf("%s %%in%% c(%s)", ids[[s$by]], paste(q(keep), collapse = ", "))
}

# Pairs of numeric columns whose order the data never break (<= 1% of rows)
# although their ranges overlap (random pairs of people break it >= 5%)
find_orders <- function(df, cols) {
    num <- cols[vapply(cols, function(v) is.numeric(df[[v]]) && length(unique(stats::na.omit(df[[v]]))) > 5, TRUE)]
    out <- list()
    for (i in seq_along(num)) for (j in seq_along(num)) {
        if (i == j) next
        a <- df[[num[i]]]; b <- df[[num[j]]]
        ok <- !is.na(a) & !is.na(b)
        if (sum(ok) < 20) next
        if (mean(a[ok] > b[ok]) > 0.01 || mean(a[ok] < b[ok]) < 0.5) next
        crossed <- with_seed(1, { k <- sample.int(sum(ok), min(sum(ok), 2000)); l <- sample.int(sum(ok), length(k))
                                  mean(a[ok][k] > b[ok][l]) })
        if (crossed >= 0.05)
            out[[length(out) + 1]] <- list(lower = num[i], upper = num[j], broken = sum(a[ok] > b[ok]))
    }
    out
}

# Messages for the person deriving (original column names)
structure_messages <- function(codes, struct, na_rows, orders, df) {
    c(vapply(names(codes), function(v) sprintf("'%s': %d values %s read as missing (a code for unknown or not applicable)",
                                               v, codes[[v]]$n, format(codes[[v]]$value)), ""),
      vapply(names(struct), function(v) {
          s <- struct[[v]]
          sprintf("'%s' does not apply when %s = %s (%d rows, %s): the scenario writes %s there (only_if/otherwise), and its effect is among the other rows",
                  v, s$by, paste(s$levels, collapse = "/"), sum(na_rows[[v]]),
                  if (is.na(s$value)) "missing" else sprintf("always %s", s$value), format(s$otherwise))
      }, ""),
      vapply(orders, function(o) sprintf("'%s' <= '%s' in every row%s: the scenario does not keep this order; write one from the other (formula) or leave one out",
                                         o$lower, o$upper, if (o$broken) sprintf(" but %d", o$broken) else ""), ""))
}

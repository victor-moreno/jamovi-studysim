# The scenario's causal diagram. dag_layout() is a layered (Sugiyama) layout
# in pixels: layers by longest path, an edge that skips layers is routed
# through dummy points in the layers between (so it never runs through a box),
# order within a layer by barycentre sweeps, and vertical positions by
# isotonic regression (each node as close to its neighbours as the order and
# the box heights allow). Effects enter the outcome box at their own ports, so
# arrows and their labels do not pile up at one point. dag_plot() draws it
# with ggplot2; dag_size() gives the image size the layout needs.

DAG_CHAR <- 7.2      # pixels per character of a node label
DAG_LCHAR <- 5.2     # pixels per character of an edge label
DAG_PAD <- 16        # vertical gap between boxes
DAG_GAP <- 80        # horizontal gap between layers (box border to box border)

# weighted isotonic regression (pool adjacent violators): non-decreasing fit to t
pava <- function(t, w = rep(1, length(t))) {
    vv <- numeric(0); ww <- numeric(0); nn <- integer(0)
    for (i in seq_along(t)) {
        vv <- c(vv, t[i]); ww <- c(ww, w[i]); nn <- c(nn, 1L)
        while (length(vv) > 1 && vv[length(vv) - 1] > vv[length(vv)]) {
            j <- length(vv)
            vv[j - 1] <- (ww[j - 1] * vv[j - 1] + ww[j] * vv[j]) / (ww[j - 1] + ww[j])
            ww[j - 1] <- ww[j - 1] + ww[j]; nn[j - 1] <- nn[j - 1] + nn[j]
            vv <- vv[-j]; ww <- ww[-j]; nn <- nn[-j]
        }
    }
    rep(vv, nn)
}

dag_layout <- function(sc, oc, factors, lang = "en") {
    vars <- sc$order
    ivs <- intersect(names(oc$effects), names(sc$interventions))
    eff <- names(oc$effects)
    depth <- stats::setNames(integer(length(vars)), vars)
    # latent paths and rule dependencies both place a variable after its parents
    links <- rbind(sc$edges[, c("from", "to")], sc$rule_edges %||% data.frame(from = character(), to = character()))
    for (v in vars) {
        pa <- links$from[links$to == v]
        if (length(pa)) depth[v] <- max(depth[pa]) + 1L
    }
    nodes <- data.frame(id = c(ivs, vars, oc$id), layer = c(rep(0L, length(ivs)), depth, max(depth) + 1L),
                        stringsAsFactors = FALSE)
    nodes$kind <- ifelse(nodes$id == oc$id, "outcome", ifelse(nodes$id %in% ivs, "intervention",
                  ifelse(nodes$id %in% factors, "measured", "unmeasured")))
    nodes$null <- !nodes$id %in% c(eff, oc$id)
    nodes$label <- dname(nodes$id, lang)
    nodes$hw <- nchar(nodes$label) * DAG_CHAR / 2 + 9
    nodes$hh <- 11
    # effects enter the outcome box at their own ports: tall enough for them all
    nodes$hh[nodes$kind == "outcome"] <- max(11, length(eff) * 9)

    # all edges with their labels; the directed ones get a chain of nodes
    edges <- list()
    add <- function(from, to, type, label, dir)
        if (length(from)) edges[[length(edges) + 1]] <<- data.frame(from = from, to = to, type = type, label = label,
                                                                      dir = dir, stringsAsFactors = FALSE)
    add(sc$edges$from, sc$edges$to, "path", sprintf("%.2f", sc$edges$b), sign(sc$edges$b))
    re <- sc$rule_edges
    if (!is.null(re) && nrow(re)) add(re$from, re$to, "path", "rule", 0)
    R <- sc$R_exo
    if (length(R) > 1) for (i in seq_len(nrow(R) - 1)) for (j in (i + 1):nrow(R)) if (R[i, j] != 0)
        add(rownames(R)[i], rownames(R)[j], "correlation", sprintf("%.2f", R[i, j]), sign(R[i, j]))
    if (length(eff)) {
        lab <- vapply(oc$effects, function(e) {
            v <- if (length(e$value) > 1) paste(signif(e$value[-1], 3), collapse = "/") else signif(e$value, 3)
            paste0(e$measure, " ", v, if (!is.null(e$by)) "*" else "")
        }, "")
        add(eff, rep(oc$id, length(eff)), "effect", unname(lab), vapply(oc$effects, function(e) sign(sum(e$coef)), 0))
    }
    edges <- if (length(edges)) do.call(rbind, edges) else NULL

    # chains through dummy nodes, one per skipped layer
    lay <- stats::setNames(nodes$layer, nodes$id)
    chains <- list()
    dums <- list()
    for (k in seq_len(NROW(edges))) {
        if (edges$type[k] == "correlation") { chains[[k]] <- c(edges$from[k], edges$to[k]); next }
        a <- lay[[edges$from[k]]]; b <- lay[[edges$to[k]]]
        mid <- if (b - a > 1) paste0("~", k, "_", (a + 1):(b - 1)) else character()
        if (length(mid)) dums[[length(dums) + 1]] <- data.frame(id = mid, layer = (a + 1):(b - 1), stringsAsFactors = FALSE)
        chains[[k]] <- c(edges$from[k], mid, edges$to[k])
    }
    all <- nodes[, c("id", "layer", "hh")]
    if (length(dums)) { d <- do.call(rbind, dums); d$hh <- 6; all <- rbind(all, d) }
    # neighbours along the chains (directed edges only)
    up <- stats::setNames(vector("list", nrow(all)), all$id); dn <- up
    for (k in seq_along(chains)) {
        if (edges$type[k] == "correlation") next
        ch <- chains[[k]]
        for (i in seq_len(length(ch) - 1)) {
            dn[[ch[i]]] <- c(dn[[ch[i]]], ch[i + 1]); up[[ch[i + 1]]] <- c(up[[ch[i + 1]]], ch[i])
        }
    }

    # order within layers: null variables last, then barycentre sweeps
    layers <- sort(unique(all$layer))
    nul <- all$id %in% nodes$id[nodes$null]
    all$pos <- 0
    for (L in layers) { i <- which(all$layer == L); all$pos[i[order(nul[i], seq_along(i))]] <- seq_along(i) }
    bary <- function(i, nb) vapply(i, function(r) {
        p <- all$pos[match(nb[[all$id[r]]], all$id)]
        if (length(p)) mean(p) else all$pos[r] }, 0)
    for (sweep in 1:6) {
        for (L in layers[-1]) { i <- which(all$layer == L); all$pos[i[order(bary(i, up), nul[i])]] <- seq_along(i) }
        for (L in rev(layers[-length(layers)])) { i <- which(all$layer == L); all$pos[i[order(nul[i], bary(i, dn))]] <- seq_along(i) }
    }

    # vertical positions (y grows downwards here): stacked, then pulled towards
    # the mean of their neighbours under the order and spacing constraints
    all$y <- 0
    sep <- function(i) { h <- all$hh[i]; c(0, cumsum(h[-1] + h[-length(h)] + ifelse(all$id[i][-1] %in% nodes$id & all$id[i][-length(i)] %in% nodes$id, DAG_PAD, 6))) }
    lord <- lapply(layers, function(L) { i <- which(all$layer == L); i[order(all$pos[i])] })
    for (i in lord) all$y[i] <- sep(i)
    nb <- lapply(all$id, function(v) c(up[[v]], dn[[v]]))
    for (it in 1:30) {
        for (i in c(lord, rev(lord))) {
            want <- vapply(i, function(r) {
                p <- all$y[match(nb[[r]], all$id)]
                if (length(p)) mean(p) else NA_real_ }, 0)
            cc <- sep(i)
            # a node without neighbours keeps its place next to the one before it
            for (j in seq_along(want)) if (is.na(want[j]))
                want[j] <- if (j > 1) want[j - 1] + cc[j] - cc[j - 1] else all$y[i[j]]
            w <- ifelse(all$id[i] %in% nodes$id, 1, 0.5)
            all$y[i] <- pava(want - cc, w) + cc
        }
    }
    top <- min(all$y - all$hh)
    all$y <- all$y - top + 8

    # x of each layer: widest box of the layer plus the gap; the gap before the
    # outcome holds the effect labels
    lw <- vapply(layers, function(L) max(c(0, nodes$hw[nodes$layer == L])), 0)
    elab <- if (!is.null(edges) && any(edges$type == "effect")) max(nchar(edges$label[edges$type == "effect"])) * DAG_LCHAR else 0
    gap <- c(rep(DAG_GAP, length(layers) - 1))
    if (length(gap)) gap[length(gap)] <- max(DAG_GAP, elab + 50)
    lx <- numeric(length(layers)); lx[1] <- 8 + lw[1]
    for (j in seq_along(layers)[-1]) lx[j] <- lx[j - 1] + lw[j - 1] + gap[j - 1] + lw[j]
    all$x <- lx[match(all$layer, layers)]
    m <- match(nodes$id, all$id)
    nodes$x <- all$x[m]; nodes$y <- all$y[m]

    # routed points of each edge: box border -> dummies -> box border (or port)
    pts <- list()
    oy <- nodes$y[nodes$kind == "outcome"]; ohh <- nodes$hh[nodes$kind == "outcome"]
    ef <- which(edges$type == "effect")
    if (length(ef)) {
        # ports in the order of the points the effects arrive from
        prev <- vapply(ef, function(k) { ch <- chains[[k]]; all$y[all$id == ch[length(ch) - 1]] }, 0)
        port <- if (length(ef) == 1) oy else seq(oy - ohh + 7, oy + ohh - 7, length.out = length(ef))
        port_of <- stats::setNames(port[rank(prev, ties.method = "first")], ef)
    }
    for (k in seq_len(NROW(edges))) {
        ch <- chains[[k]]
        r <- match(ch, all$id)
        x <- all$x[r]; y <- all$y[r]
        n1 <- match(ch[1], nodes$id); n2 <- match(ch[length(ch)], nodes$id)
        if (edges$type[k] == "correlation") {
            # between two boxes of the same layer: from border to border, vertically
            s <- sign(y[2] - y[1])
            if (x[1] == x[2]) { y[1] <- y[1] + s * nodes$hh[n1]; y[2] <- y[2] - s * nodes$hh[n2] }
            else { x[1] <- x[1] + nodes$hw[n1]; x[2] <- x[2] - nodes$hw[n2] }
        } else {
            x[1] <- x[1] + nodes$hw[n1]
            x[length(x)] <- x[length(x)] - nodes$hw[n2] - 2
            if (edges$type[k] == "effect") y[length(y)] <- port_of[[as.character(k)]]
        }
        pts[[k]] <- data.frame(x = x, y = y)
    }
    if (!is.null(edges)) {
        edges$x <- vapply(pts, function(p) p$x[1], 0); edges$y <- vapply(pts, function(p) p$y[1], 0)
        edges$xend <- vapply(pts, function(p) p$x[nrow(p)], 0); edges$yend <- vapply(pts, function(p) p$y[nrow(p)], 0)
    }
    if (!is.null(edges)) {
        lab <- dag_labels(edges, pts, nodes)
        edges$lx <- lab$lx; edges$ly <- lab$ly
    }
    list(nodes = nodes, edges = edges, points = pts,
         width = max(nodes$x + nodes$hw) + 8, height = max(all$y + all$hh) + 8)
}

# Label positions along each edge's curve, greedily away from boxes and labels
# already placed: effect labels first, in the middle of their last stretch
# (one per port, so they never collide), then path labels, tried at several
# points of each stretch; correlation labels beside their bow.
dag_labels <- function(edges, pts, nodes) {
    bw <- function(l) nchar(l) * DAG_LCHAR / 2 + 3
    boxes <- data.frame(x = nodes$x, y = nodes$y, hw = nodes$hw, hh = nodes$hh)
    free <- function(x, y, hw) !any(abs(boxes$x - x) < boxes$hw + hw & abs(boxes$y - y) < boxes$hh + 7)
    lx <- ly <- rep(NA_real_, nrow(edges))
    on <- function(q, i, t) c(q$x[i] + t * (q$x[i + 1] - q$x[i]), q$y[i] + (q$y[i + 1] - q$y[i]) * t * t * (3 - 2 * t))
    ord <- c(which(edges$type == "effect"), which(edges$type == "path"), which(edges$type == "correlation"))
    for (k in ord) {
        q <- pts[[k]]; hw <- bw(edges$label[k])
        cand <- if (edges$type[k] == "effect") list(on(q, nrow(q) - 1, 0.55))
                else if (edges$type[k] == "correlation") list(c((q$x[1] + q$x[2]) / 2 - 18, (q$y[1] + q$y[2]) / 2))
                else unlist(lapply(seq_len(nrow(q) - 1), function(i)
                    lapply(c(0.5, 0.35, 0.65, 0.25, 0.75, 0.42, 0.58), function(t) on(q, i, t))), recursive = FALSE)
        pick <- cand[[1]]
        for (cpos in cand) if (free(cpos[1], cpos[2], hw)) { pick <- cpos; break }
        lx[k] <- pick[1]; ly[k] <- pick[2]
        boxes <- rbind(boxes, data.frame(x = pick[1], y = pick[2], hw = hw, hh = 0))
    }
    list(lx = lx, ly = ly)
}

# image size: the layout plus the plot margins and the caption's lines
dag_size <- function(g, caption = "") {
    w <- ceiling(g$width) + 20
    lines <- if (nzchar(caption)) length(strwrap(caption, width = max(60, round(w / 7)))) else 0
    c(w = w, h = ceiling(g$height) + 20 + lines * 14 + if (lines) 8 else 0)
}

# smooth curve through the routed points: an S-shaped cubic between each pair,
# horizontal at both ends (so it passes the dummy points level)
dag_curve <- function(p, n = 16) {
    if (nrow(p) < 2) return(p)
    out <- lapply(seq_len(nrow(p) - 1), function(i) {
        t <- seq(0, 1, length.out = n)[if (i == 1) seq_len(n) else -1]
        s <- t * t * (3 - 2 * t)
        data.frame(x = p$x[i] + (p$x[i + 1] - p$x[i]) * t, y = p$y[i] + (p$y[i + 1] - p$y[i]) * s)
    })
    do.call(rbind, out)
}

dag_plot <- function(g, w, h, key = FALSE, caption = "") {
    nodes <- g$nodes; edges <- g$edges
    flip <- function(y) -y
    p <- ggplot2::ggplot() +
        ggplot2::coord_cartesian(xlim = c(0, g$width), ylim = flip(c(g$height, 0)), expand = FALSE)
    arr <- grid::arrow(length = grid::unit(2.2, "mm"), type = "closed")
    if (!is.null(edges)) {
        cols <- ifelse(edges$type == "effect",
                       if (key) ifelse(edges$dir > 0, "#c0392b", ifelse(edges$dir < 0, "#2e6da4", "grey20")) else "grey20",
                       "grey60")
        curves <- do.call(rbind, lapply(seq_len(nrow(edges)), function(k) {
            cp <- dag_curve(g$points[[k]])
            cp$k <- k; cp$col <- cols[k]; cp$type <- edges$type[k]
            cp$lw <- if (edges$type[k] == "effect") 0.6 else 0.45
            cp
        }))
        co <- curves[curves$type == "correlation", ]
        di <- curves[curves$type != "correlation", ]
        if (nrow(co)) {
            # correlations: a dotted bow beside the two boxes
            ce <- edges[edges$type == "correlation", ]
            p <- p + ggplot2::geom_curve(data = ce, ggplot2::aes(x = x, y = flip(y), xend = xend, yend = flip(yend)),
                                         curvature = 0.35, linetype = "dotted", colour = "grey55")
        }
        if (nrow(di)) p <- p + ggplot2::geom_path(data = di, ggplot2::aes(x = x, y = flip(y), group = k, colour = col, linewidth = lw),
                                                  arrow = arr, linejoin = "round")
        if (key) p <- p + ggplot2::geom_label(data = edges, ggplot2::aes(x = lx, y = flip(ly), label = label), size = 2.7,
                                             label.size = 0, label.padding = grid::unit(0.08, "lines"), fill = "white", colour = "grey25")
    }
    nodes$lt <- ifelse(nodes$kind == "unmeasured", "dashed", "solid")
    nodes$fill <- ifelse(nodes$kind == "outcome", "#2e5f8f", ifelse(nodes$kind == "intervention", "#f6e3c6", "white"))
    nodes$tc <- ifelse(nodes$kind == "outcome", "white", ifelse(nodes$null | nodes$kind == "unmeasured", "grey40", "black"))
    nodes$ff <- ifelse(nodes$null, "italic", ifelse(nodes$kind == "outcome", "bold", "plain"))
    p + ggplot2::geom_tile(data = nodes, ggplot2::aes(x = x, y = flip(y), width = 2 * hw, height = 2 * hh, fill = fill, linetype = lt),
                           colour = "grey30", linewidth = 0.4) +
        ggplot2::geom_text(data = nodes, ggplot2::aes(x = x, y = flip(y), label = label, colour = tc, fontface = ff), size = 3.4) +
        ggplot2::scale_fill_identity() + ggplot2::scale_linetype_identity() + ggplot2::scale_colour_identity() +
        ggplot2::scale_linewidth_identity() +
        ggplot2::labs(caption = paste(strwrap(caption, width = max(60, round(w / 7))), collapse = "\n")) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9, colour = "grey35"),
                       plot.margin = ggplot2::margin(10, 10, 10, 10))
}

utils::globalVariables(c("xend", "yend", "k", "lw", "col", "lx", "ly", "label", "x", "y", "hw", "hh", "fill", "lt", "tc", "ff"))

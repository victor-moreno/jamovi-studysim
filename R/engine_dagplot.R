# Drawing of dag_layout() with ggplot2: nodes as boxes (dashed when not
# measured), arrows clipped at the box borders, effect labels only when the
# answer key is shown. w and h are the image size in pixels, used to turn the
# boxes' pixel size into data units.

dag_plot <- function(g, w, h, key = FALSE, caption = "") {
    nodes <- g$nodes; edges <- g$edges
    xr <- range(nodes$x) + c(-0.6, 0.6); yr <- range(nodes$y) + c(-0.7, 0.7)
    pxx <- (w - 40) / diff(xr); pxy <- (h - 90) / diff(yr)        # pixels per data unit
    nodes$hw <- (nchar(nodes$label) * 3.6 + 9) / pxx                 # half box sizes in data units
    nodes$hh <- 11 / pxy
    clip <- function(e) {
        if (is.null(e) || nrow(e) == 0) return(e)
        a <- match(e$from, nodes$id); b <- match(e$to, nodes$id)
        dx <- (e$xend - e$x) * pxx; dy <- (e$yend - e$y) * pxy
        frac <- function(i) pmin(abs(nodes$hw[i] * pxx / dx), abs(nodes$hh[i] * pxy / dy), na.rm = TRUE)
        fa <- frac(a) + 0.01; fb <- frac(b) + 0.03
        e$x0 <- e$x + fa * (e$xend - e$x); e$y0 <- e$y + fa * (e$yend - e$y)
        e$x1 <- e$xend - fb * (e$xend - e$x); e$y1 <- e$yend - fb * (e$yend - e$y)
        e
    }
    edges <- clip(edges)
    p <- ggplot2::ggplot() + ggplot2::coord_cartesian(xlim = xr, ylim = yr, expand = FALSE)
    if (!is.null(edges)) {
        pa <- edges[edges$type == "path", ]
        co <- edges[edges$type == "correlation", ]
        ef <- edges[edges$type == "effect", ]
        arr <- grid::arrow(length = grid::unit(2.2, "mm"), type = "closed")
        if (nrow(co)) p <- p + ggplot2::geom_curve(data = co, ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1),
                                                   curvature = 0.35, linetype = "dotted", colour = "grey55")
        if (nrow(pa)) p <- p + ggplot2::geom_segment(data = pa, ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1),
                                                     colour = "grey60", arrow = arr)
        if (nrow(ef)) {
            ef$col <- if (key) ifelse(ef$dir > 0, "#c0392b", ifelse(ef$dir < 0, "#2e6da4", "grey20")) else "grey20"
            # effects that skip layers bow outwards, so they do not run through the nodes in between
            long <- (ef$xend - ef$x) > 1.5
            for (cv in c(-0.12, 0.12)) {
                d <- ef[long & (if (cv > 0) ef$y >= ef$yend else ef$y < ef$yend), ]
                if (nrow(d)) p <- p + ggplot2::geom_curve(data = d, ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1, colour = col),
                                                          curvature = cv, arrow = arr, linewidth = 0.6)
            }
            if (any(!long)) p <- p + ggplot2::geom_segment(data = ef[!long, ], ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1, colour = col),
                                                           arrow = arr, linewidth = 0.6)
        }
        if (key) {
            lab <- edges
            t <- ifelse(lab$type == "effect", 0.22, 0.5)
            lab$lx <- lab$x0 + t * (lab$x1 - lab$x0); lab$ly <- lab$y0 + t * (lab$y1 - lab$y0)
            p <- p + ggplot2::geom_label(data = lab, ggplot2::aes(x = lx, y = ly, label = label), size = 2.7,
                                         label.size = 0, label.padding = grid::unit(0.08, "lines"), fill = "white", colour = "grey25")
        }
    }
    nodes$lt <- ifelse(nodes$kind == "unmeasured", "dashed", "solid")
    nodes$fill <- ifelse(nodes$kind == "outcome", "#2e5f8f", ifelse(nodes$kind == "intervention", "#f6e3c6", "white"))
    nodes$tc <- ifelse(nodes$kind == "outcome", "white", ifelse(nodes$null | nodes$kind == "unmeasured", "grey40", "black"))
    nodes$ff <- ifelse(nodes$null, "italic", ifelse(nodes$kind == "outcome", "bold", "plain"))
    p + ggplot2::geom_tile(data = nodes, ggplot2::aes(x = x, y = y, width = 2 * hw, height = 2 * hh, fill = fill, linetype = lt),
                           colour = "grey30", linewidth = 0.4) +
        ggplot2::geom_text(data = nodes, ggplot2::aes(x = x, y = y, label = label, colour = tc, fontface = ff), size = 3.4) +
        ggplot2::scale_fill_identity() + ggplot2::scale_linetype_identity() + ggplot2::scale_colour_identity() +
        ggplot2::labs(caption = paste(strwrap(caption, width = max(60, round(w / 7))), collapse = "\n")) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9, colour = "grey35"),
                       plot.margin = ggplot2::margin(10, 10, 10, 10))
}

utils::globalVariables(c("x0", "y0", "x1", "y1", "col", "lx", "ly", "label", "x", "y", "hw", "hh", "fill", "lt", "tc", "ff"))

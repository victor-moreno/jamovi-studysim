# Figures of the epidemiology guide: DAGs, flowcharts and design schematics,
# drawn with ggplot2 on a transparent background, once for the light and once
# for the dark page theme (a cell with `renderings: [light, dark]` calls both()).
library(ggplot2)
knitr::opts_chunk$set(dev = "ragg_png", dpi = 192, dev.args = list(bg = "transparent"))

# colours that read on both themes; roles of variables in a DAG
PAL <- function(mode) {
    dark <- mode == "dark"
    list(fg = if (dark) "#e4e4e1" else "#202326", muted = if (dark) "#a3a9b0" else "#5f656c",
         faint = if (dark) "#4b5157" else "#cdd1d5", panel = if (dark) "#262a2f" else "#f3f2ee",
         role = c(exposure = "#3b82c4", outcome = "#d0473c", confounder = "#d98c1a", mediator = "#2f9d68",
                  collider = "#9b59b6", modifier = "#7d8a96", instrument = "#1a9c9c", other = "#8a8f96",
                  case = "#d0473c", control = "#3b82c4"))
}

theme_fig <- function(mode, base = 12) {
    p <- PAL(mode)
    theme_void(base_size = base) +
        theme(text = element_text(colour = p$fg), plot.title = element_text(face = "bold", size = base + 1, hjust = 0),
              plot.subtitle = element_text(colour = p$muted), strip.text = element_text(colour = p$fg, face = "bold", size = base),
              legend.text = element_text(colour = p$fg), legend.title = element_text(colour = p$muted),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.background = element_rect(fill = "transparent", colour = NA))
}

# axes and grid for data plots
theme_plot <- function(mode, base = 12) {
    p <- PAL(mode)
    theme_minimal(base_size = base) +
        theme(text = element_text(colour = p$fg), axis.text = element_text(colour = p$muted),
              panel.grid.major = element_line(colour = p$faint, linewidth = 0.3), panel.grid.minor = element_blank(),
              strip.text = element_text(colour = p$fg, face = "bold"), legend.position = "bottom",
              plot.background = element_rect(fill = "transparent", colour = NA))
}

# print a figure for the light and for the dark theme
both <- function(f, ...) for (m in c("light", "dark")) print(f(m, ...))

# ---- DAGs ------------------------------------------------------------------------
# nodes: id, x, y, role (see PAL), label (optional), panel (optional)
# edges: from, to, curve (0 = straight), dashed (TRUE/FALSE), label (optional)
# adjusted: ids drawn with a double border (conditioned on); unmeasured: dashed border
dag_fig <- function(mode, nodes, edges, adjusted = character(), unmeasured = character(), ncol = NULL,
                    title = NULL, subtitle = NULL, legend = FALSE, base = 12) {
    p <- PAL(mode)
    nodes$label <- if (is.null(nodes$label)) nodes$id else ifelse(is.na(nodes$label), nodes$id, nodes$label)
    if (is.null(nodes$panel)) nodes$panel <- ""
    if (is.null(edges$panel)) edges$panel <- ""
    if (is.null(edges$curve)) edges$curve <- 0
    if (is.null(edges$dashed)) edges$dashed <- FALSE
    if (is.null(edges$label)) edges$label <- ""
    nodes$w <- 0.16 * pmax(nchar(nodes$label), 2) + 0.35
    nodes$h <- 0.42
    key <- function(panel, id) paste(panel, id, sep = "\r")
    nk <- key(nodes$panel, nodes$id)
    a <- nodes[match(key(edges$panel, edges$from), nk), ]; b <- nodes[match(key(edges$panel, edges$to), nk), ]
    # clip each arrow at the border of the boxes it joins
    clip <- function(n, dx, dy) {
        t <- pmin(ifelse(dx == 0, Inf, (n$w / 2 + 0.04) / abs(dx)), ifelse(dy == 0, Inf, (n$h / 2 + 0.04) / abs(dy)))
        pmin(t, 0.8)
    }
    dx <- b$x - a$x; dy <- b$y - a$y
    ta <- clip(a, dx, dy); tb <- clip(b, dx, dy)
    # boxes so close that the clipped ends would cross: share what is left
    tot <- ta + tb
    k <- ifelse(tot > 0.92, 0.92 / tot, 1)
    ta <- ta * k; tb <- tb * k
    e <- data.frame(x = a$x + ta * dx, y = a$y + ta * dy, xend = b$x - tb * dx, yend = b$y - tb * dy,
                    curve = edges$curve, dashed = edges$dashed, label = edges$label, panel = edges$panel)
    nodes$fill <- p$role[nodes$role]
    nodes$adj <- nodes$id %in% adjusted
    nodes$unm <- nodes$id %in% unmeasured
    g <- ggplot()
    for (cv in unique(e$curve)) {
        ec <- e[e$curve == cv, ]
        geom <- if (cv == 0) geom_segment else function(...) geom_curve(..., curvature = cv)
        g <- g + geom(data = ec, aes(x = x, y = y, xend = xend, yend = yend, linetype = dashed), colour = p$muted, linewidth = 0.7,
                      arrow = arrow(length = unit(2.6, "mm"), type = "closed"))
    }
    el <- e[nzchar(e$label), ]
    if (nrow(el)) g <- g + geom_label(data = el, aes(x = (x + xend) / 2, y = (y + yend) / 2, label = label), colour = p$fg,
                                      fill = if (mode == "dark") "#1f2226" else "#ffffff", label.size = 0, size = base / 3.3)
    g <- g +
        geom_rect(data = nodes[nodes$adj, ], aes(xmin = x - w / 2 - 0.07, xmax = x + w / 2 + 0.07, ymin = y - h / 2 - 0.07, ymax = y + h / 2 + 0.07),
                  fill = NA, colour = p$fg, linewidth = 0.6) +
        geom_rect(data = nodes, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, linetype = unm),
                  fill = scales::alpha(nodes$fill, 0.22), colour = nodes$fill, linewidth = 0.8) +
        geom_text(data = nodes, aes(x = x, y = y, label = label), colour = p$fg, size = base / 3.1) +
        scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") +
        coord_equal(clip = "off") + theme_fig(mode, base) + labs(title = title, subtitle = subtitle)
    if (length(unique(nodes$panel)) > 1) g <- g + facet_wrap(~ factor(panel, levels = unique(nodes$panel)), ncol = ncol)
    if (legend) {
        r <- unique(nodes$role)
        lg <- data.frame(role = factor(r, levels = r), x = mean(range(nodes$x)), y = min(nodes$y))
        g <- g + geom_point(data = lg, aes(x = x, y = y, colour = role), alpha = 0, size = 4) +
            scale_colour_manual(values = p$role[r], name = NULL) +
            guides(colour = guide_legend(override.aes = list(alpha = 0.9, shape = 15, size = 5), nrow = 1)) +
            theme(legend.position = "bottom")
    }
    g
}

# ---- flowcharts ------------------------------------------------------------------
# boxes: id, x, y, label, (role) ; links: from, to
flow_fig <- function(mode, boxes, links, w = 2.2, h = 0.75, base = 11, title = NULL) {
    p <- PAL(mode)
    if (is.null(boxes$role)) boxes$role <- "other"
    if (is.null(boxes$w)) boxes$w <- w
    a <- boxes[match(links$from, boxes$id), ]; b <- boxes[match(links$to, boxes$id), ]
    horiz <- abs(b$x - a$x) > abs(b$y - a$y)
    e <- data.frame(x = ifelse(horiz, a$x + sign(b$x - a$x) * a$w / 2, a$x), y = ifelse(horiz, a$y, a$y + sign(b$y - a$y) * h / 2),
                    xend = ifelse(horiz, b$x - sign(b$x - a$x) * b$w / 2, b$x), yend = ifelse(horiz, b$y, b$y - sign(b$y - a$y) * h / 2))
    ggplot() +
        geom_segment(data = e, aes(x = x, y = y, xend = xend, yend = yend), colour = p$muted, linewidth = 0.7,
                     arrow = arrow(length = unit(2.4, "mm"), type = "closed")) +
        geom_rect(data = boxes, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2),
                  fill = scales::alpha(p$role[boxes$role], 0.18), colour = p$role[boxes$role], linewidth = 0.7) +
        geom_text(data = boxes, aes(x = x, y = y, label = label), colour = p$fg, size = base / 3.2, lineheight = 0.9) +
        coord_equal(clip = "off") + theme_fig(mode, base) + labs(title = title)
}

# ---- design schematics -------------------------------------------------------------

# people followed in time: one line per person from entry to the end of
# follow-up, an event as a cross, censoring as a bar. d: start, end, event (0/1), group
timeline_fig <- function(mode, d, title = NULL, xlab = "years", marks = NULL, highlight = NULL, base = 11) {
    p <- PAL(mode)
    d$row <- seq_len(nrow(d))
    cols <- c(p$role[["exposure"]], p$role[["other"]])
    g <- ggplot(d) +
        geom_segment(aes(x = start, xend = end, y = row, yend = row, colour = group), linewidth = 1.1,
                     alpha = if (is.null(highlight)) 1 else ifelse(d$row %in% highlight, 1, 0.3)) +
        geom_point(data = d[d$event == 1, ], aes(x = end, y = row), shape = 4, size = 3, stroke = 1.3, colour = p$role[["outcome"]]) +
        geom_point(data = d[d$event == 0, ], aes(x = end, y = row), shape = 124, size = 3.5, colour = p$muted) +
        scale_colour_manual(values = setNames(cols[seq_along(unique(d$group))], unique(d$group)), name = NULL,
                            guide = if (length(unique(d$group)) > 1) "legend" else "none") +
        scale_y_continuous(breaks = NULL) + labs(x = xlab, y = NULL, title = title) + theme_plot(mode, base) +
        theme(panel.grid.major.y = element_blank())
    if (!is.null(marks)) g <- g + marks(p)
    g
}

# the source population as dots, and who a design samples from it.
# pop: x, y, status (case / non-case), panel, chosen (TRUE / FALSE / NA = everybody alike)
sample_fig <- function(mode, pop, title = NULL, base = 11) {
    p <- PAL(mode)
    pop$panel <- factor(pop$panel, levels = unique(pop$panel))
    out <- !is.na(pop$chosen) & !pop$chosen
    sz <- ifelse(is.na(pop$chosen), 1.5, 2.6)
    ggplot(pop, aes(x = x, y = y)) +
        geom_point(data = pop[out, ], colour = p$faint, size = 1.1) +
        geom_point(data = pop[!out, ], aes(colour = status), size = sz[!out]) +
        scale_colour_manual(values = c(case = p$role[["case"]], `non-case` = p$role[["other"]]), name = NULL) +
        facet_wrap(~panel) + coord_equal() + theme_fig(mode, base) + theme(legend.position = "bottom") + labs(title = title)
}

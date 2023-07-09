# Make an ordered factor without reordering input
ordfac <- \(x)factor(x, levels = unique(x), ordered = TRUE)

# Unlike scales::rescale, values are capped at the lower and upper bounds
rescale <- function(x, bounds) {
  u <- max(bounds)
  l <- min(bounds)
  x <- pmin(u, x, na.rm = FALSE)
  x <- pmax(l, x, na.rm = FALSE)
  (x-l)/(u-l)
}

rhat_theme <- list(
  palette = c("blue", "white", "red", "black"),
  heat = list(ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.border = ggplot2::element_rect(colour = "black", fill=NA,
      linewidth=2)),
    ggplot2::xlab(""), ggplot2::ylab(""),
    ggplot2::guides(linetype = "none"),
    ggplot2::scale_x_discrete(expand = c(0,0)),
    ggplot2::scale_y_discrete(expand = c(0,0))),
  violin = ggplot2::theme(
    strip.text.x = ggplot2::element_text(angle = 90),
    strip.text.y.left = ggplot2::element_text(angle = 0),
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = -45, hjust = 1),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.spacing.x = grid::unit(0.1, "lines"),
    panel.spacing.y = grid::unit(0.1, "lines"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = "gray80"),
    panel.grid.major.y = ggplot2::element_blank()
))
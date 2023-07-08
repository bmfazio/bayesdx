#' @export
rhat_violin <- \(data, bounds = c(1, 1.5), expscale = 0.25, displace = -1.05,
                 colors = rhat_theme$palette) {
  data$trans <- rescale(data$rhat, bounds)**expscale + displace
  data$y <- "" # geom_violin complains if no 'y' is provided
  # values and labels can also probably be constructed from args
  values <- c(0, 0.02, 0.1, 1)**expscale
  labels <- c("1", "1.01", "1.05", "1.5+")
  # use same order as input
  data$rows <- ordfac(data$rows)
  data$cols <- ordfac(data$cols)
  ggplot2::ggplot(data, ggplot2::aes(x = trans, y = y, color = trans)
  ) + ggplot2::geom_violin(alpha = 0.5, fill = "gray40", color = "gray50"
  ) + ggbeeswarm::geom_quasirandom(size = 1, method = "tukey", width = 0.25
  ) + ggplot2::scale_x_continuous(position = "top", breaks = values[2:3],
        labels = NULL
  ) + ggplot2::scale_color_gradientn(colors = colors, values = values,
        breaks = values, labels = labels, limits = c(0,1)
  ) + ggplot2::facet_grid(rows = ggplot2::vars(rows),cols = ggplot2::vars(cols),
        switch = "both"
  ) + rhat_theme$violin
}

#' @export
rhat_heat <- \(data, bounds = c(1, 1.5), expscale = 0.25, displace = -1.05,
              colors = rhat_theme$palette) {
  data <- dplyr::group_by(data, rows, cols) |>
  dplyr::summarise(rhat = mean(rhat, na.rm = TRUE)) |>
  dplyr::mutate(rhat = rescale(data$rhat, bounds)**expscale + displace)
  # plotting pars
  values <- c(0, 0.02, 0.1, 1)**expscale
  labels <- c("1", "1.01", "1.05", "1.5+")
  # use same order as input
  data$rows <- ordfac(data$rows)
  data$cols <- ordfac(data$cols)
  ggplot2::ggplot(data, ggplot2::aes(x = variable, y = sim_desc, fill = rhat)
  ) + ggplot2::geom_tile(
  ) + ggplot2::scale_fill_gradientn(colors = colors, values = values,
    breaks = values, labels = labels, limits = c(0,1)
  ) + rhat_theme$heat
}
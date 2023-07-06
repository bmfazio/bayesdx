#' @export
add_rmse <- function(x) {
  lapply(unique(x$stats$sim_id), \(i){ 
    dplyr::left_join(
    dplyr::filter(x$stats[,1:3], sim_id == i),
    tidyr::pivot_longer(as.data.frame(posterior::as_draws_df(x$fits[[i]])),
      cols = !dplyr::starts_with(".")),
    by = c("variable" = "name")) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(rmse = sqrt(mean((simulated_value - value)**2))) |>
    dplyr::bind_cols(sim_id = i)
  }) |> dplyr::bind_rows() |>
  dplyr::left_join(x$stats, y = _, by = c("sim_id", "variable"))
}
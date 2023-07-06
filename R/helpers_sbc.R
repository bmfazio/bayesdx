### probably should make a generalized add_x that takes a summary function
#' @export
rmse <- function(x, y)sqrt(mean((x - y)**2))

#' @export
add_summary <- function(x, f) {
  lapply(unique(x$stats$sim_id), \(i){ 
    dplyr::left_join(
    dplyr::filter(x$stats[,1:3], sim_id == i),
    tidyr::pivot_longer(as.data.frame(posterior::as_draws_df(x$fits[[i]])),
      cols = !dplyr::starts_with(".")),
    by = c("variable" = "name")) |>
    dplyr::group_by(variable) |>
    eval(parse(text = paste0("dplyr::summarise(", paste(sapply(f,
      \(z)paste0(z, " = ", z, "(simulated_value, value)")),
      collapse = ", " ), ")"))) |>
    dplyr::bind_cols(sim_id = i)
  }) |> dplyr::bind_rows() |>
  dplyr::left_join(x$stats, y = _, by = c("sim_id", "variable"))
}

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
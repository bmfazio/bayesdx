rmse <- function(x, y)sqrt(mean((x - y)**2))
bias <- function(x, y)mean(x - y)

# Classic measures for parameter recovery
#' @export
bdx_recovery <- function(x){
  bdx_add_summaries(x, list(bias = bias, rmse = rmse))
}

#' @export
bdx_add_summaries <- function(x, f) {
  funs <- \(j,k){as.data.frame(lapply(f, \(g)g(j, k)))}
  lapply(unique(x$stats$sim_id), \(i){ 
    dplyr::left_join(
    dplyr::filter(x$stats[,1:3], sim_id == i),
    tidyr::pivot_longer(as.data.frame(posterior::as_draws_df(x$fits[[i]])),
      cols = !dplyr::starts_with(".")),
    by = c("variable" = "name")) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(funs(value, simulated_value)) |>
    dplyr::bind_cols(sim_id = i)
  }) |> dplyr::bind_rows() |>
  dplyr::left_join(x$stats, y = _, by = c("sim_id", "variable"))
}
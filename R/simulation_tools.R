library(future)
library(future.apply)
plan(multiprocess)
n_sim <- 100
n_obs <- c(100, 1000)
mu <- 2

sim_results <- lapply(n_obs, function(sample_size) {
  estimator_sim <- future_lapply(seq_len(n_sim), function(iter) {
    y_obs <- rnorm(sample_size, mu)
    estimate <- mean(y_obs)
    return(estimate)
  })
  estimates <- do.call(rbind, estimator_sim)
  #n_samp <- rep(sample_size, n_sim)
  #out <- as.data.frame(list(n_samp = n_samp, estimates = estimates))
  return(estimates)
})

#' ...
mc_inference <- function(simulation_results, rm_na = TRUE) {
  # type safety
  assertthat::assert_that(class(simulation_results) == "list")

  # remove missing values from vector or data.frame
  if (rm_na) {
    simulation_results <- lapply(simulation_results,
                                 function(x) x[stats::complete.cases(x), ])
  }

  # compute simulation statistics
  results_with_cis <- lapply(simulation_results, function(results) {
    results_stats <- results %>%
      as.numeric() %>%
      enframe() %>%
      mutate(
        param_est = mean(value),
        param_var = var(value),
        ci_lwr = param_est - (param_var * abs(qnorm(p = (1 - ci_level) / 2))),
        ci_upr = param_est + (param_var * abs(qnorm(p = (1 - ci_level) / 2))),
        covers = (ci_lwr <= truth & truth <= ci_upr),
      )
    return(results_stats)
  })
}

#' ...
mc_summarize <- function(...) {
  sim_res_summary <- results_with_cis %>%
    group_by(n_samp) %>%
    summarise(
      n_sim = n(),                         # how many complete simulations
      bias_avg = mean(param_est - truth),  # subtract truth for bias
      est_mc_var = var(param_est),
      se_avg = mean(param_var),
      mse = (bias_avg)^2 + est_mc_var,
      cvr = mean(covers)
    )
}

#' ...
mc_error_summarize <- function(...) {
  sim_res_errors <- results_with_cis %>%
    group_by(n_samp) %>%
    summarise(
      n_sim = n(),
      bias_err = sqrt(var(param_est - truth) / n_sim) *
        abs(qnorm(p = (1 - ci_level) / 2)),
      est_mc_var_err = sqrt(var((param_est - mean(param_est))^2) / n_sim) *
        abs(qnorm(p = (1 - ci_level) / 2)),
      mse_err = sqrt(var(var(param_est) + (param_est - truth)^2) / n_sim) *
        abs(qnorm(p = (1 - ci_level) / 2))
    )
}

#' ...
summarize_simulations <- function() {
    left_join(sim_res_summary, sim_res_errors)
}

#' ...
plot.simulation_statistics <- function() {
    ...
}

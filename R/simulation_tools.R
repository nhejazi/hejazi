library(tidyverse)
n_sim <- 100
n_obs <- c(100, 10000)
mu <- 2

sim_results <- lapply(n_obs, function(sample_size) {
  estimator_sim <- lapply(seq_len(n_sim), function(iter) {
    y_obs <- rnorm(sample_size, mu)
    est_param <- mean(y_obs)
    est_var <- var(y_obs)
    estimate <- as_tibble(list(param_est = est_param, param_var = est_var))
    return(estimate)
  })
  estimates <- do.call(rbind, estimator_sim)
  # n_samp <- rep(sample_size, n_sim)
  # out <- as.data.frame(list(n_samp = n_samp, estimates = estimates))
  return(estimates)
})

#' ...
mc_summarize <- function(simulation_results, truth, ci_level = 0.95) {
  # check that parameter estimate and variance estimate are the only columns
  assertthat::assert_that(all.equal(colnames(simulation_results),
                                    c("param_est", "param_var")))

  # compute simulation-based inference
  results_with_cis <- simulation_results %>%
    mutate(
      param_var = param_var / sqrt(n()),
      ci_lwr = param_est - (param_var * abs(qnorm(p = (1 - ci_level) / 2))),
      ci_upr = param_est + (param_var * abs(qnorm(p = (1 - ci_level) / 2))),
      covers = (ci_lwr <= truth & truth <= ci_upr),
    )

  # compute simulation statistics
  sim_res_summary <- results_with_cis %>%
    summarise(
      n_sim = n(),                        # how many complete simulations
      bias_avg = mean(param_est - truth), # subtract truth for bias
      est_mc_var = var(param_est),
      se_avg = mean(param_var),
      mse = (bias_avg)^2 + est_mc_var,
      cvr = mean(covers)
    )

  # compute errors of simulation statistics
  # NOTE: useful for constructing intervals around summary measures
  sim_res_errors <- results_with_cis %>%
    summarise(
      n_sim = n(),
      bias_err = sqrt(var(param_est - truth) / n_sim) *
        abs(qnorm(p = (1 - ci_level) / 2)),
      est_mc_var_err = sqrt(var((param_est - mean(param_est))^2) / n_sim) *
        abs(qnorm(p = (1 - ci_level) / 2)),
      mse_err = sqrt(var(var(param_est) + (param_est - truth)^2) / n_sim) *
        abs(qnorm(p = (1 - ci_level) / 2))
    )

  # output
  return(list(results = results_with_cis,
              summary_stat = sim_res_summary,
              summary_stat_error = sim_res_errors))
}

#' ...
plot.simulation_statistics <- function() {
  ...
}


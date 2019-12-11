utils::globalVariables(c(
  "param_est", "param_var", "param_se", "ci_lwr", "ci_upr", "n_sim",
  "err", "est", "type"
))

#' Summarize Simulations Results
#'
#' @param simulation_results A \code{data.frame}, \code{tibble} or similar with
#'  exactly two columns named \code{"param_est"} and \code{"param_var"} giving
#'  the estimate of a parameter of interest and estimate of its variance (based
#'  on a valid variance estimator specific to that parameter).Each row of this
#'  data structure corresponds to the parameter estimate and variance for a
#'  single iteration of several simulations.
#' @param truth A \code{numeric} value giving the true value of the parameter
#'  of interest in the simulation setting.
#' @param ci_level A \code{numeric} value giving the level of the confidence
#'  intervals to be generated around the parameter estimates and statistics
#'  computed to summarize the simulation.
#'
#' @importFrom stats var qnorm
#' @importFrom assertthat assert_that
#' @importFrom dplyr "%>%" mutate transmute summarise n
#'
#' @export
#'
#' @examples
#' n_sim <- 1000
#' n_obs <- c(100, 10000)
#' mu <- 2
#' sim_results <- lapply(n_obs, function(sample_size) {
#'   estimator_sim <- lapply(seq_len(n_sim), function(iter) {
#'     y_obs <- rnorm(sample_size, mu)
#'     est_param <- mean(y_obs)
#'     est_var <- var(y_obs) / sample_size
#'     estimate <- tibble::as_tibble(list(
#'       param_est = est_param,
#'       param_var = est_var
#'     ))
#'     return(estimate)
#'   })
#'   estimates <- do.call(rbind, estimator_sim)
#'   return(estimates)
#' })
#' sim_summary <- lapply(sim_results, summarize_sim, truth = mu)
summarize_sim <- function(simulation_results,
                          truth,
                          ci_level = 0.95) {
  # check that parameter estimate and variance estimate are the only columns
  assertthat::assert_that(all.equal(
    colnames(simulation_results),
    c("param_est", "param_var")
  ))

  # multiplier for creating confidence intervals
  ci_mult <- abs(stats::qnorm(p = (1 - ci_level) / 2))

  # compute simulation-based inference
  results_with_cis <- simulation_results %>%
    dplyr::mutate(
      param_se = sqrt(param_var),
      ci_lwr = param_est - param_se * ci_mult,
      ci_upr = param_est + param_se * ci_mult,
      covers = (ci_lwr <= truth & truth <= ci_upr)
    )

  # compute simulation statistics and errors of simulation statistics
  # NOTE: useful for constructing intervals around summary measures
  ## summary statistics for bias and relevant statistics
  bias_summary <- results_with_cis %>%
    dplyr::summarise(
      n_sim = dplyr::n(),
      est = mean(param_est - truth),
      err = sqrt(stats::var(param_est - truth) / n_sim),
      ci_lwr = est - err * ci_mult,
      ci_upr = est + err * ci_mult,
      type = "bias"
    )
  ## summary statistics for Monte Carlo variance and relevant statistics
  var_summary <- results_with_cis %>%
    dplyr::summarise(
      n_sim = dplyr::n(),
      est = mean((param_est - mean(param_est))^2),
      err = sqrt(stats::var((param_est - mean(param_est))^2) / n_sim),
      ci_lwr = est - err * ci_mult,
      ci_upr = est + err * ci_mult,
      type = "mc_var"
    )
  ## summary statistics for MSE and relevant statistics
  mse_summary <- results_with_cis %>%
    dplyr::summarise(
      n_sim = dplyr::n(),
      est = (mean(param_est - truth))^2 + stats::var(param_est),
      err = sqrt(stats::var((param_est - truth)^2 + stats::var(param_est)) /
        n_sim),
      ci_lwr = est - err * ci_mult,
      ci_upr = est + err * ci_mult,
      type = "mse"
    )
  # create output object with simulation summary statistics
  sim_summary <- rbind(bias_summary, var_summary, mse_summary) %>%
    dplyr::transmute(
      lwr_ci = ci_lwr,
      stat_est = est,
      upr_ci = ci_upr,
      stat_type = type
    )

  # output
  out <- list(
    results_sim = results_with_cis,
    summary_sim = sim_summary
  )
  class(out) <- "simulation_stats"
  return(out)
}

################################################################################

utils::globalVariables(c("stat_est", "stat_type", "lwr_ci", "upr_ci", "n_samp"))

#' Visualize Summaries of Simulation Results
#'
#' @param x A \code{list} of several simulation summary objects, of class
#'  \code{simulation_stats}.
#' @param ... Extra arguments currently ignored.
#' @param sample_sizes A \code{numeric} vector giving the sample sizes at which
#'  each of the simulations in the input \code{x} was performed. There should be
#'  one unique sample size corresponding to each element of \code{x}.
#' @param stat A \code{character} indicating which of three simulation summary
#'  statistics for which to generate a plot. Options are currently limited to
#'  bias (\code{"bias"}), variance (\code{"mc_var"}), and mean-squared error
#'  (\code{"mse"}).
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr "%>%" mutate filter
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline
#' @importFrom ggplot2 xlab ylab ggtitle theme_bw theme
#'
#' @export
#' @examples
#' n_sim <- 100
#' n_obs <- c(100, 10000)
#' mu <- 2
#' sim_results <- lapply(n_obs, function(sample_size) {
#'   estimator_sim <- lapply(seq_len(n_sim), function(iter) {
#'     y_obs <- rnorm(sample_size, mu)
#'     est_param <- mean(y_obs)
#'     est_var <- var(y_obs)
#'     estimate <- tibble::as_tibble(list(
#'       param_est = est_param,
#'       param_var = est_var
#'     ))
#'     return(estimate)
#'   })
#'   estimates <- do.call(rbind, estimator_sim)
#'   return(estimates)
#' })
#' sim_summary <- lapply(sim_results, summarize_sim, truth = mu)
#' p_sim_summary <- sim_plot(sim_summary, sample_sizes = n_obs, stat = "mse")
#' p_sim_summary
sim_plot <- function(x,
                     ...,
                     sample_sizes,
                     stat = c("bias", "mc_var", "mse")) {
  # check arguments
  stat <- match.arg(stat)
  assertthat::assert_that(length(sample_sizes) == length(x))

  # extract simulation summary slot in input list
  sim_summaries <- x %>%
    lapply(function(x) {
      x[["summary_sim"]]
    })

  # create single data table to hold simulation summaries
  sim_comparison <- do.call(rbind, sim_summaries) %>%
    dplyr::mutate(
      n_samp = rep(sample_sizes, each = 3),
      n = NULL
    )

  # build plot from simulation comparison table
  p_sim_summary <- sim_comparison %>%
    dplyr::filter(stat_type == stat) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = as.factor(n_samp), y = stat_est,
      colour = stat_type
    )) +
    ggplot2::geom_point(size = 4, alpha = 0.75) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr_ci, ymax = upr_ci),
      width = 0.05, linetype = "dashed"
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::xlab("Sample size") +
    ggplot2::ylab("") +
    ggplot2::ggtitle("") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  # return plot object as output
  return(p_sim_summary)
}

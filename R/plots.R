utils::globalVariables(c("ord.x", "z", "lower", "upper", "label"))

#' Quantile-Quantile Plots
#'
#' Produce standard quantile-quantile plots for modeling using ggplot2.
#'
#' @param x A numeric vector of residuals from a generalized linear model.
#' @param distribution The reference probability distribution for residuals.
#' @param ... Any additional parameters to be passed to distribution functions.
#' @param conf The confidence level to be used with confidence intervals.
#' @param labels The names to be used when identifying points on the Q-Q plot.
#' @param line.estimate Should quantiles be estimated, if so which quantiles?
#'
#' @importFrom stats quantile ppoints na.omit qnorm coef
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab ggtitle theme
#' @importFrom ggplot2 geom_abline geom_text geom_ribbon scale_size_continuous
#'
#' @export
#'
#' @examples
#' n <- 100
#' x1 <- rnorm(n)
#' y1 <- rnorm(n)
#' linmod <- lm(y1 ~ x1)
#' x <- linmod$residuals
#' qq_plot(x)
qq_plot <- function(x, distribution = "norm", ..., line.estimate = NULL,
                    conf = 0.95, labels = names(x)) {
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))

  if (is.null(line.estimate)) {
    Q.x <- stats::quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- (diff(Q.x) / diff(Q.z))
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- stats::coef(line.estimate(ord.x ~ z))
  }

  zz <- stats::qnorm(1 - ((1 - conf) / 2))
  SE <- (coef[2] / d.function(df$z)) * sqrt(P * ((1 - P) / n))
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE

  if (!is.null(labels)) {
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower,
      labels[ord], ""
    )
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = z, y = ord.x)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = coef[1], slope = coef[2]) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.2
    ) +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab("Model Residual Quantiles") +
    ggplot2::ggtitle("Quantile Plot Residuals") +
    ggplot2::theme_bw()
  if (!is.null(labels)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = label))
  }
  return(p)
}


################################################################################

utils::globalVariables(c(
  ".fitted", ".resid", ".stdresid", ".cooksd", ".hat"
))

#' Linear Model Diagnostic Plots
#'
#' Produce standard diagnostic plots for linear models using ggplot2.
#'
#' @param x A linear model object produced by \code{lm()}.
#' @param ... Extra arguments, currently ignored.
#'
#' @importFrom ggplot2 ggplot aes geom_point stat_smooth geom_hline geom_abline
#' @importFrom ggplot2 xlab ylab ggtitle geom_bar scale_size_continuous theme
#' @importFrom gridExtra grid.arrange
#'
#' @export
#'
#' @examples
#' n <- 100
#' x1 <- rnorm(n)
#' y1 <- rnorm(n)
#' linmod <- lm(y1 ~ x1)
#' plot(linmod)
lm_plot <- function(x, ...) {
  p1 <- ggplot2::ggplot(x, ggplot2::aes(x = .fitted, y = .resid)) +
    ggplot2::geom_point() +
    ggplot2::stat_smooth(method = "loess", se = FALSE) +
    ggplot2::geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    ggplot2::xlab("Fitted vals") +
    ggplot2::ylab("Residuals") +
    ggplot2::ggtitle("Residual vs. Fitted") +
    ggplot2::theme_bw()

  p2 <- ggplot2::ggplot(x, ggplot2::aes(sample = .stdresid)) +
    ggplot2::geom_point(stat = "qq") +
    ggplot2::geom_abline() +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab("Std. Residuals") +
    ggplot2::ggtitle("Gausian Q-Q") +
    ggplot2::theme_bw()

  p3 <- ggplot2::ggplot(x, ggplot2::aes(
    x = .fitted,
    y = sqrt(abs(.stdresid))
  )) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::stat_smooth(method = "loess", se = FALSE, na.rm = TRUE) +
    ggplot2::xlab("Fitted Value") +
    ggplot2::ylab(expression(sqrt("|Std. residuals|"))) +
    ggplot2::ggtitle("Scale-Location") +
    ggplot2::theme_bw()

  p4 <- ggplot2::ggplot(x, ggplot2::aes(
    x = seq_along(.cooksd),
    y = .cooksd
  )) +
    ggplot2::geom_bar(stat = "identity", position = "identity") +
    ggplot2::xlab("Obs. No.") +
    ggplot2::ylab("Cook's distance") +
    ggplot2::ggtitle("Cook's distance") +
    ggplot2::theme_bw()

  p5 <- ggplot2::ggplot(x, ggplot2::aes(x = .hat, y = .stdresid)) +
    ggplot2::geom_point(ggplot2::aes(size = .cooksd), na.rm = TRUE) +
    ggplot2::stat_smooth(method = "loess", se = FALSE, na.rm = TRUE) +
    ggplot2::xlab("Leverage") +
    ggplot2::ylab("Std. Resid.") +
    ggplot2::ggtitle("Resid. vs Leverage") +
    ggplot2::scale_size_continuous("Cook's Distance", range = c(1, 5)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme_bw()

  p6 <- ggplot2::ggplot(x, ggplot2::aes(x = .hat, y = .cooksd)) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::stat_smooth(method = "loess", se = FALSE, na.rm = TRUE) +
    ggplot2::xlab("Leverage") +
    ggplot2::ylab("Cook's Distance") +
    ggplot2::ggtitle("Cook's dist. vs. Lev.") +
    ggplot2::geom_abline(
      slope = seq(0, 3, 0.5), color = "gray",
      linetype = "dashed"
    ) +
    ggplot2::theme_bw()

  return(gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3))
}

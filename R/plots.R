utils::globalVariables(c("ord.x", "z", "lower", "upper", "label"))

#' Quantile-Quantile Plots with ggplot2
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
#' @importFrom stats quantile ppoints na.omit qnorm
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab ggtitle theme
#' @importFrom ggplot2 geom_abline geom_text geom_ribbon scale_size_continuous
#'
#' @export qqPlot_gg
#'
#' @examples
#' n <- 100; x1 <- rnorm(n); y1 <- rnorm(n);
#' linmod <- lm(y1 ~ x1)
#' x <- linmod$residuals
#' qqPlot_gg(x)

qqPlot_gg <- function(x, distribution = "norm", ..., line.estimate = NULL,
                      conf = 0.95, labels = names(x)) {
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))

  if(is.null(line.estimate)) {
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- (diff(Q.x) / diff(Q.z))
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }

  zz <- qnorm(1 - ( (1 - conf) / 2))
  SE <- (coef[2] / d.function(df$z)) * sqrt(P * ( (1 - P) / n))
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE

  if(!is.null(labels)) {
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower,labels[ord],"")
  }

  p <- ggplot(df, aes(x = z, y = ord.x)) +
    geom_point() +
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    ylab("Model Residual Quantiles") + xlab("Theoretical Quantiles") +
    ggtitle("Quantile Plot Residuals")
  if(!is.null(labels)) {
    p <- p + geom_text( aes(label = label))
  }
  return(p)
}


#####################################
########### NEXT FUNCTION ###########
#####################################

utils::globalVariables(c("aes", ".fitted", ".resid", "geom_point", ".stdresid",
                       "geom_hline", "xlab", "ylab", "ggtitle", "stat_smooth",
                       "geom_abline", ".cooksd", "geom_bar", ".hat", "theme",
                       "scale_size_continuous"))

#' Linear Model Diagnostic Plots with ggplot2
#'
#' Produce standard diagnostic plots for linear models using ggplot2.
#'
#' @param model A linear model object produced by \code{lm()}.
#'
#' @importFrom ggplot2 ggplot aes geom_point stat_smooth geom_hline geom_abline
#' @importFrom ggplot2 xlab ylab ggtitle geom_bar scale_size_continuous theme
#' @importFrom gridExtra grid.arrange
#'
#' @export lmPlots_gg
#'
#' @examples
#' n <- 100; x1 <- rnorm(n); y1 <- rnorm(n);
#' linmod <- lm(y1 ~ x1)
#' lmPlots_gg(linmod)

lmPlots_gg <- function(model) {
  p1 <- ggplot(model, aes(x = .fitted, y = .resid)) + geom_point() +
        stat_smooth(method = "loess") +
        geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
        xlab("Fitted vals") + ylab("Residuals") + ggtitle("Residual vs. Fitted")

  p2 <- ggplot(model, aes(sample = .stdresid)) + geom_point(stat = "qq") +
        geom_abline() + xlab("Theoretical Quantiles") + ylab("Std. Residuals") +
        ggtitle("Gausian Q-Q")

  p3 <- ggplot(model, aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
        geom_point(na.rm = TRUE) + stat_smooth(method = "loess", na.rm = TRUE) +
        xlab("Fitted Value") + ylab(expression(sqrt("|Std. residuals|"))) +
        ggtitle("Scale-Location")

  p4 <- ggplot(model, aes(x = seq_along(.cooksd), y = .cooksd)) +
        geom_bar(stat = "identity", position = "identity") +
        xlab("Obs. No.") + ylab("Cook's distance") + ggtitle("Cook's distance")

  p5 <- ggplot(model, aes(x = .hat, y = .stdresid)) +
        geom_point(aes(size = .cooksd), na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) + xlab("Leverage") +
        ggtitle("Resid. vs Leverage") + ylab("Std. Resid.") +
        scale_size_continuous("Cook's Distance", range = c(1,5)) +
        theme(legend.position = "none")

  p6 <- ggplot(model, aes(x = .hat, y = .cooksd)) + geom_point(na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) + xlab("Leverage") +
        ylab("Cook's Distance") + ggtitle("Cook's dist. vs. Lev.") +
        geom_abline(slope = seq(0,3,0.5), color = "gray", linetype = "dashed")

  return(gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3))
}

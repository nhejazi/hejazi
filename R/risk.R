#' Mean Squared Error
#'
#' Compute the mean squared error (risk under L2 loss).
#'
#' @param prediction A \code{numeric} vector of predictions.
#' @param outcome A \code{numeric} vector of outcomes actually observed.
#'
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' y <- x^2
#' test_x <- rnorm(100)
#' test_y <- test_x^2
#' mod <- glm(y ~ x)
#' pred <- predict(mod, newx = as.data.frame(test_x))
#' error <- mse(prediction = pred, outcome = test_y)
mse <- function(prediction, outcome) {
  emp_loss <- (prediction - outcome)^2
  return(mean(emp_loss))
}

#' Risk for Cross-Entropy Loss
#'
#' Compute the empirical risk under cross-entropy loss for binary predictions.
#'
#' @param prediction A \code{numeric} vector of predicted probabilities.
#' @param outcome A \code{numeric} vector of binary outcomes actually observed.
#'
#' @export
#'
#' @examples
#' n_obs <- 100
#' x <- rnorm(n_obs)
#' y <- rbinom(n_obs, 1, plogis(x^2))
#' test_x <- rnorm(n_obs)
#' test_y <- rbinom(n_obs, 1, plogis(test_x^2))
#' mod <- glm(y ~ x, family = "binomial")
#' pred <- predict(mod, newx = as.data.frame(test_x), type = "response")
#' error <- nll(prediction = unname(pred), outcome = test_y)
nll <- function(prediction, outcome) {
  emp_loss <- (-log(prediction) * outcome) + (-log(1 - prediction) *
    (1 - outcome))
  return(mean(emp_loss))
}

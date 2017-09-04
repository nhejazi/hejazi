#' Mean Squared Error (MSE)
#'
#' Easily compute the mean squared error for continuous predictions
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
#'
mse <- function(prediction, outcome) {
  mean((prediction - outcome)^2)
}


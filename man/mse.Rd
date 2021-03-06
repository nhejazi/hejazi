% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/risk.R
\name{mse}
\alias{mse}
\title{Mean Squared Error}
\usage{
mse(prediction, outcome)
}
\arguments{
\item{prediction}{A \code{numeric} vector of predictions.}

\item{outcome}{A \code{numeric} vector of outcomes actually observed.}
}
\description{
Compute the mean squared error (risk under L2 loss).
}
\examples{
x <- rnorm(100)
y <- x^2
test_x <- rnorm(100)
test_y <- test_x^2
mod <- glm(y ~ x)
pred <- predict(mod, newx = as.data.frame(test_x))
error <- mse(prediction = pred, outcome = test_y)
}

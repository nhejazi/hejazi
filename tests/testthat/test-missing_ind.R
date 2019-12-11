context("Missingness indicators appended appropriately")
library(stringr)

test_that("missingness indicators appears as expected", {
  missingness <- c(3, 4)
  data <- data.frame(cbind(rnorm(10), runif(10)))
  data[sample(nrow(data), missingness[1]), 1] <- NA
  data[sample(nrow(data), missingness[2]), 2] <- NA
  data <- miss_ind(data)
  na_detected <- unname(colSums(data[, str_detect(colnames(data), "miss_")]))
  expect_equal(missingness, na_detected)
})

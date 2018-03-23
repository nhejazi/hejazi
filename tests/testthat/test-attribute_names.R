context("Checking whether attribute names are properly extracted")

p <- 5
n <- 100

test_that("attrnames finds the names of the attributes of a matrix", {
  x <- matrix(seq_len(n), ncol = p)
  colnames(x) <- LETTERS[seq_len(p)]
  expect_true(
    all(attrnames(x) %in% c("dim", "dimnames"))
  )
})

test_that("attrnames finds the names of the attributes of a data frame", {
  x <- matrix(seq_len(n), ncol = p)
  colnames(x) <- LETTERS[seq_len(p)]
  y <- data.frame(x)
  expect_true(
    all(attrnames(y) %in% c("names", "row.names", "class"))
  )
})

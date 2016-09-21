library(nima)
context("Checking whether attribute names are properly extracted")

test_that("attrnames finds the names of the attributes of a matrix", {
	x <- matrix(1:100, ncol = 5)
	colnames(x) <- LETTERS[1:5]
	expect_equal(attrnames(x), 
		     	       c("dim", "dimnames") )
})

test_that("attrnames finds the names of the attributes of a data frame", {
	x <- matrix(1:100, ncol = 5)
	colnames(x) <- LETTERS[1:5]
	y <- data.frame(x)
	expect_equal(attrnames(y), 
		     	       c("names", "row.names", "class") )
})

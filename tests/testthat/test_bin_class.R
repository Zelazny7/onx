
context("Bin Class")


test_that("Bin constructors work", {
  
  expect_s4_class(BinNumeric(lower=0, upper=100), "BinNumeric")
  expect_error(BinNumeric(lower=10, upper=10))
  expect_error(BinNumeric(lower=10, upper=0))
  
  expect_equal(BinNumeric()@lower, -Inf)
  expect_equal(BinNumeric()@upper, Inf)
  
  expect_s4_class(BinFactor(level="level1"), "BinFactor")
  expect_equal(BinFactor()@level, "")
  
  expect_s4_class(BinMissing(), "BinMissing")  
  
})

test_that("Boolean masks", {
  
  a <- BinNumeric(lower=-5, upper=5)
  expect_equal(get_boolean_mask(a, 1:10), c(T, T, T, T, T, F, F, F, F, F))
  expect_equal(get_boolean_mask(a, -5:0), c(F, T, T, T, T, T))
  
  b <- BinFactor(level="cat")
  expect_equal(get_boolean_mask(b, factor(c("dog", "cat"))), c(F, T))
  expect_equal(get_boolean_mask(b, factor(c("cat", "cat"))), c(T, T))
  expect_equal(get_boolean_mask(b, factor(c("CAT", "CAT"))), c(F, F))
  
  c <- BinMissing()  
  expect_equal(get_boolean_mask(c, c(1, 2, NA)), c(F, F, T))
  expect_equal(get_boolean_mask(c, c(NA, NA, NA)), c(T, T, T))
  expect_equal(get_boolean_mask(c, factor(c(NA, "cat"))), c(T, F))
  
  
})

test_that("Combine BinNumeric", {
  
  a <- BinNumeric(lower=1, upper=10)
  b <- BinNumeric(lower=7, upper=15)
  c <- BinNumeric(lower=15, upper=20)
  
  expect_equal(is_valid_combination(a, b), TRUE)
  expect_equal(is_valid_combination(b, a), TRUE)
  expect_equal(is_valid_combination(a, c), FALSE)
  expect_equal(is_valid_combination(c, a), FALSE)
  
  expect_equal(combine_bins(a, b), BinNumeric(lower=1, upper=15))
  expect_equal(combine_bins(a, c), list(a, c))

  
  
})


test_that("Bin labels", {
  
  a <- BinNumeric(lower=1, upper=10)
  b <- BinFactor(level="Factor Level")
  c <- BinMissing()
  
  expect_equal(get_label(a), "(1 - 10]")
  expect_equal(get_label(b), "Factor Level")
  expect_equal(get_label(c), "Missing")
  
  
})


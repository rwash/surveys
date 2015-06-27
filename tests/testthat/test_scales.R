library(surveys)
context("scales helper functions")

test_that("standardize returns mean 0, sd 1", {
  out <- standardize(rnorm(100, 5, 2))
  expect_that(mean(out), equals(0))
  expect_that(sd(out), equals(1))
  expect_that(length(out), equals(100))
})




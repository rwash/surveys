library(surveys)
context("question type functions")

test_that("numeric detector works", {
  expect_true(char_is_numeric(c("1.0", "2.0", "3", "-4", "-5.5", "12", "12.001", NA)))
  expect_false(char_is_numeric(c("1.0", "hi")))
  expect_false(char_is_numeric(c("1.0", "1.0hi")))
})

test_that("numeric detection works", {
  out <- detect.question(c("1.0", "2.0", "3.5"))
  expect_equal(out, c(1,2,3.5))
  out <- detect.question(c("-2", "-2.5", "11"))
  expect_equal(out, c(-2, -2.5, 11))
})

test_that("logical detector works", {
  expect_true(char_is_logical(c("True", "true", "TRUE", "T", "False", "F", "false", "FALSE", "0", "1", NA)))
  expect_false(char_is_logical(c("True", "hi")))
  expect_false(char_is_logical(c("True", "1.0x")))
})

test_that("convert_logical works", {
  expect_equal(convert_logical(c("T", "F")), c("T", "F"))
  expect_equal(convert_logical(c("1", "0")), c("T", "F"))
})

test_that("logical detection works", {
  out <- detect.question(c("TRUE", "FALSE", "TRUE"))
  expect_equal(out, c(T,F,T))
  out <- detect.question(c("F", "F", "T"))
  expect_equal(out, c(F,F,T))
  out <- detect.question(c("1", "0", "0"))
  expect_equal(out, c(T,F,F))
  out <- detect.question(c("T", "F", NA))
  expect_equal(out, c(T,F,NA))
})

test_that("ip detector works", {
  expect_true(char_is_ip(c("1.2.3.4", "127.0.0.1", "11.2.3.4", NA)))
  expect_false(char_is_ip(c("1")))
  expect_false(char_is_ip(c("1.2")))
  expect_false(char_is_ip(c("1.2.3")))
})

test_that("ip detection works", {
  out <- detect.question(c("1.2.3.4", "127.0.0.1", NA))
  expect_equal(out, c("1.2.3.4", "127.0.0.1", NA))
})




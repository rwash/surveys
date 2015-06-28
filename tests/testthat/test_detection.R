library(surveys)
context("question type detection")

test_that("questions get added to the environment", {
  # Run something from the package
  checkbox <- function(x) { sapply(x, isTRUE) }
  add_question_type("test_checkbox", is.logical, checkbox)
  expect_true(exists("test_checkbox", envir=surveys::question_types))
  expect_is(get("test_checkbox", envir=surveys::question_types), "list")
  val = get("test_checkbox", envir=surveys::question_types)
  expect_is(val[[1]], "function")
  expect_is(val[[2]], "function")
})

test_that("repeat questions throw errors", {
  add_question_type("test_repeat", is.numeric, as.numeric)
  expect_warning(surveys::add_question_type("test_repeat", is.numeric, as.numeric), "already")
})

test_that("it correctly detects a simple type", {
  add_question_type("test_numeric", is.numeric, as.numeric)
  out <- detect.question(c(1.0, 2.0, 4.0))
  expect_equal(length(out), 3)
  expect_equal(out[1], 1)
  expect_equal(out[2], 2)
  expect_equal(out[3], 4)
  expect_is(out, "numeric")
})

test_that("unknown questions throw an error", {
  input <- c("hello", "world")
  expect_warning(out <- detect.question(input), "unknown")
  expect_equal(out, input)
})

test_that("processing a data frame works", {
  char_is_numeric <- function(x) { return(all(grepl("^[-+]?[1-90]+(\\.[1-90]*)?$", x) | is.na(x))) }
  add_question_type("test_numeric2", char_is_numeric, as.numeric)
  df <- data.frame(one= c("1.0", "2.0"), two=c("3.0", "4.0"), stringsAsFactors=F)
  out <- detect.survey(df)
  expect_is(out, "data.frame")
  expect_equal(length(out), 2)
  expect_equal(length(out[,1]), 2)
  expect_equal(length(out[,2]), 2)
  expect_is(out[,1], "numeric")
  expect_is(out[,2], "numeric")
  expect_equal(out[,1], c(1, 2))
  expect_equal(out[,2], c(3, 4))
})

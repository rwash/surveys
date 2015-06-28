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
  rm(list="test_checkbox", envir=surveys::question_types)
})

test_that("repeat questions throw errors", {
  add_question_type("test_repeat", is.numeric, as.numeric)
  expect_warning(surveys::add_question_type("test_repeat", is.numeric, as.numeric), "already")
  rm(list="test_repeat", envir=surveys::question_types)
})

test_that("questions can be added by name", {
  add_question_type("test_by_name", "is.logical", "as.numeric")
  expect_true(exists("test_by_name", envir=surveys::question_types))
  expect_is(get("test_by_name", envir=surveys::question_types), "list")
  val = get("test_by_name", envir=surveys::question_types)
  expect_is(val[[1]], "function")
  expect_is(val[[2]], "function")
  rm(list="test_by_name", envir=surveys::question_types)
})

test_that("question types can be removed", {
  qtype = "test_removal"
  add_question_type(qtype, is.numeric, as.numeric)
  expect_true(exists(qtype, envir=surveys::question_types))
  remove_question_type(qtype)
  expect_false(exists(qtype, envir=surveys::question_types))
})

test_that("remove column processors can be added", {
  add_question_type("test_remove_column", is.null, "remove")
  expect_true(exists("test_remove_column", envir=surveys::question_types))
  expect_is(get("test_remove_column", envir=surveys::question_types), "list")
  val = get("test_remove_column", envir=surveys::question_types)
  expect_is(val[[1]], "function")
  expect_is(val[[2]], "character")
  expect_equal(val[[2]], "remove")
  rm(list="test_remove_column", envir=surveys::question_types)
})

test_that("it correctly detects a simple type", {
  add_question_type("test_numeric", is.numeric, as.numeric)
  out <- detect.question(c(1.0, 2.0, 4.0))
  expect_equal(length(out), 3)
  expect_equal(out[1], 1)
  expect_equal(out[2], 2)
  expect_equal(out[3], 4)
  expect_is(out, "numeric")
  rm(list="test_numeric", envir=surveys::question_types)
})

test_that("unknown questions throw an error", {
  input <- c("hello", "world")
  expect_warning(out <- detect.question(input), "unknown")
  expect_equal(out, input)
})

test_that("removing a column with a processor works", {
  is.ab <- function(x) { return(all(grepl("a|b", x) | is.na(x)))}
  add_question_type("test_remove_column_detect", is.ab, "remove")
  out <- detect.question(c("a", "b"))
  expect_null(out)
  out2 <- detect.question(c("T", "F"))
  expect_equal(length(out2), 2)
  rm(list="test_remove_column_detect", envir=surveys::question_types)
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
  rm(list="test_numeric2", envir=surveys::question_types)
})

test_that("removing from a data frame works", {
  is.ab <- function(x) { return(all(grepl("a|b", x) | is.na(x)))}
  add_question_type("test_remove_column_detect", is.ab, "remove")
  df <- data.frame(one= c("a", "b"), two=c("3.0", "4.0"), stringsAsFactors=F)
  out <- detect.survey(df)
  expect_is(out, "data.frame")
  expect_equal(length(out), 1)
  expect_equal(length(out[,1]), 2)
  expect_is(out[,1], "numeric")
  expect_equal(out[,1], c(3, 4))
  rm(list="test_remove_column_detect", envir=surveys::question_types)
})

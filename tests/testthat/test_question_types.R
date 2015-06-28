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

test_that("quatrics subject_id detector works", {
  expect_true(is_qualtrics_subject_id(c("R_abcdeabcdeabcde", "R_ab123ab123ab123", "R_123451234512345")))
  expect_false(is_qualtrics_subject_id(c("abcdeabcdeabcde", "R_abcde")))
})

test_that("qualtrics subject_id detection works", {
  ids <- c("R_abcdeabcdeabcde", "R_ab123ab123ab123", "R_123451234512345")
  out <- detect.question(ids)
  expect_equal(out, ids)
})

test_that("single value columns are detected", {
  expect_true(all_identical(c("1", "1", "1")))
  expect_true(all_identical(c("a", "a", "a")))
  expect_true(all_identical(c("", "", "")))
  expect_true(all_identical(c("xyz", "xyz", "xyz")))
  expect_false(all_identical(c("1", "1", "2")))
})

test_that("single value columns are removed", {
  # TODO: fill this test in
})

test_that("date detector works", {
  expect_true(char_is_date(c("2015-04-05 15:23:15", "2015-06-28 11:45:01")))
  expect_false(char_is_date(c("20157-04-05 15:23:15", "2015-06-28 11:45:01")))
  expect_false(char_is_date(c("2015-04-05 25:23:15", "2015-06-28 11:45:01")))
})

test_that("date detection works", {
  dates <- c("2015-04-05 15:23:15", "2015-06-28 11:45:01")
  out <- detect.question(dates)
  expect_is(out, "POSIXct")
  expect_equal(out[1], lubridate::ymd_hms("2015-04-05 15:23:15"))
  expect_equal(out[2], lubridate::ymd_hms("2015-06-28 11:45:01"))
})

test_that("adding an attention check question works", {
  add_attention_check("test_ac1", "test_valid")
  expect_true(exists("test_ac1", envir=surveys::attention_checks))
  expect_equal(get("test_ac1", envir=surveys::attention_checks), "test_valid")
  rm(list="test_ac1", envir=surveys::attention_checks)
})

test_that("attention check detection works in detect.question", {
  add_attention_check("test_ac2", "test_valid")
  out <- detect.question(c("test_valid", "test_invalid"), col_name="test_ac2")
  expect_is(out, "logical")
  expect_equal(length(out), 2)
  expect_true(out[1])
  expect_false(out[2])
  rm(list="test_ac2", envir=surveys::attention_checks)
})

test_that("attention check detection works in detect.survey", {
  add_attention_check("test_ac3", "test_valid")
  df <- data.frame(one=c("1.0", "2.0", "2.0"), test_ac3=c("test_invalid", "test_valid", "test_valid"), stringsAsFactors = F)
  out <- detect.survey(df)
  expect_is(out, "data.frame")
  expect_is(out$test_ac3, "logical")
  expect_equal(length(out$test_ac3), 3)
  expect_false(out$test_ac3[1])
  expect_true(out$test_ac3[2])
  expect_true(out$test_ac3[3])
  rm(list="test_ac3", envir=surveys::attention_checks)
})



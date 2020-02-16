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
  expect_true(all_identical(c(NA, NA, NA)))
  expect_false(all_identical(c("1", "1", "2")))
})

test_that("single value columns are marked for removal", {
  expect_null(detect.question(c("1", "1", "1", "1"), "test_id1"))
  expect_null(detect.question(c(NA, NA, NA), "test_id2"))
  expect_null(detect.question(c("", "", "", "", ""), "test_id3"))
})

test_that("single value columns are removed", {
  df <- data.frame(one=c("1", "2", "3"), two=c("1", "1", "1"), three=c("", "", ""))
  out <- detect.survey(df)
  expect_equal(length(out), 1)
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
  expect_true(exists("test_ac1", envir=attention_checks))
  expect_equal(get("test_ac1", envir=attention_checks), "test_valid")
  rm(list="test_ac1", envir=attention_checks)
})

test_that("attention checks fail NAs", {
  add_attention_check("test_ac4", "test_valid")
  col <- c("test_valid", "test_invalid", NA, "test_valid")
  attr(col, "name") <- "test_ac4"
  out <- attention_check(col)
  expect_is(out, "logical")
  expect_equal(length(out), 4)
  expect_true(out[1])
  expect_false(out[2])
  expect_false(out[3])
  expect_true(out[4])
  rm(list="test_ac4", envir=attention_checks)
})

test_that("attention check detection works in detect.question", {
  add_attention_check("test_ac2", "test_valid")
  out <- detect.question(c("test_valid", "test_invalid"), col_name="test_ac2")
  expect_is(out, "logical")
  expect_equal(length(out), 2)
  expect_true(out[1])
  expect_false(out[2])
  rm(list="test_ac2", envir=attention_checks)
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
  rm(list="test_ac3", envir=attention_checks)
})

test_that("adding an ignore question", {
  ignore_question("test_ignore")
  expect_true(exists("test_ignore", envir=ignore_questions))
  rm(list="test_ignore", envir=ignore_questions)
})

test_that("ignored questions are ignored by detect.question", {
  ignore_question("test_ignore2")
  out <- detect.question(c("1.0", "2.0"), col_name="test_ignore2")
  expect_is(out, "character")
  expect_equal(out[1], "1.0")
  expect_equal(out[2], "2.0")
  rm(list="test_ignore2", envir=ignore_questions)
})

test_that("ignored questions are ignored by detect.survey", {
  ignore_question("test_ignore3")
  df <- data.frame(one=c("1.0", "2.0", "2.0"), test_ignore3=c("1.0", "2.2", "3.3"), stringsAsFactors = F)
  out <- detect.survey(df)
  expect_is(out, "data.frame")
  expect_is(out$test_ignore3, "character")
  expect_equal(length(out$test_ignore3), 3)
  expect_equal(out$test_ignore3[1], "1.0")
  expect_equal(out$test_ignore3[2], "2.2")
  expect_equal(out$test_ignore3[3], "3.3")
  rm(list="test_ignore3", envir=ignore_questions)
})

test_that("checkboxes can be detected", {
  expect_true(char_is_checkbox(c("Hi", "Hi", NA, NA)))
  expect_false(char_is_checkbox(c("Hi", "Hi")))
  expect_false(char_is_checkbox(c(NA,NA)))
})

test_that("converting checkboxes is correct", {
  out <- checkbox(c("Hi", "Hi", NA, NA))
  expect_equal(length(out), 4)
  expect_is(out, "logical")
  expect_equal(out, c(T,T,F,F))
})

test_that("checkboxes are detected by detect.question", {
  out <- detect.question(c("test", "test", NA, NA, "test"), col_name="test_cbox")
  expect_is(out, "logical")
  expect_equal(length(out), 5)
  expect_equal(out, c(T,T,F,F,T))
})

test_that("checkboxes are detected by detect.survey", {
  df <- data.frame(one=c("1.0", "2.0", "2.0", "2.5", "2.5"), two=c("test", "test", NA, NA, "test"), stringsAsFactors = F)
  out <- detect.survey(df)
  expect_is(out, "data.frame")
  expect_is(out$two, "logical")
  expect_equal(length(out$two), 5)
  expect_equal(out$two, c(T,T,F,F,T))
})

test_that("adding an known question works", {
  known_question("test_kq1", c("Yes", "No"))
  expect_true(exists("test_kq1", envir=known_questions))
  temp <- get("test_kq1", envir=known_questions)
  expect_equal(temp[['options']], NULL)
  expect_equal(temp[['levels']], c("Yes", "No"))
  expect_false(temp[['ordered']])
  rm(list="test_kq1", envir=known_questions)
})

test_that("known question detection works in detect.question", {
  known_question("test_kq2", c("test_valid", "test_invalid"))
  out <- detect.question(c("test_valid", "test_invalid", "test_valid"), col_name="test_kq2")
  expect_is(out, "factor")
  expect_equal(length(out), 3)
  expect_equal(levels(out), c("test_valid", "test_invalid"))
  expect_false(is.ordered(out))
  rm(list="test_kq2", envir=known_questions)
})

test_that("known question detection works in detect.survey", {
  known_question("test_kq3", c("test_valid", "test_invalid"))
  df <- data.frame(one=c("1.0", "2.0", "2.0"), test_kq3=c("test_invalid", "test_valid", "test_valid"), stringsAsFactors = F)
  out <- detect.survey(df)
  expect_is(out, "data.frame")
  expect_is(out$test_kq3, "factor")
  expect_equal(length(out$test_kq3), 3)
  expect_equal(levels(out$test_kq3), c("test_valid", "test_invalid"))
  expect_false(is.ordered(out$test_kq3))
  rm(list="test_kq3", envir=known_questions)
})

test_that("known question detection works with specified ordered levels", {
  known_question("test_kq5", c("test_valid", "test_invalid"), options=c(5,7), ordered=T)
  out <- detect.question(c(5,7,5), col_name="test_kq5")
  expect_is(out, "factor")
  expect_equal(length(out), 3)
  expect_equal(levels(out), c("test_valid", "test_invalid"))
  expect_true(is.ordered(out))
  rm(list="test_kq5", envir=known_questions)
})

# test_that("adding a multiple answer question works", {
#   multiple_answer_question(c("ma_1", "ma_2"))
#   expect_true(exists("test_ma1", envir=mulanswer_questions))
#   temp <- get("test_ma1", envir=mulanswer_questions)
#   expect_equal(temp[['options']], NULL)
#   expect_equal(temp[['levels']], c("Yes", "No"))
#   rm(list="test_ma1", envir=mulanswer_questions)
# })
#
# test_that("multiple answer question detection works in detect.question", {
#   multiple_answer_question("test_ma2", c("Yes", "No"))
#   out <- detect.question(c("Yes", "No", "Yes,No"), col_name="test_ma2")
#   expect_is(out, "list")
#   expect_equal(length(out), 3)
#   expect_equal(length(out[[1]]), 1)
#   expect_equal(out[[1]], "Yes")
#   expect_equal(length(out[[2]]), 1)
#   expect_equal(out[[2]], "No")
#   expect_equal(length(out[[3]]), 2)
#   expect_equal(out[[3]], c("Yes", "No"))
#   rm(list="test_ma2", envir=mulanswer_questions)
# })
#
# test_that("mulitple answer question detection works in detect.survey", {
#   multiple_answer_question("test_ma3", c("Yes", "No"))
#   df <- data.frame(one=c("1.0", "2.0", "2.0"), test_ma3=c("Yes", "No", "Yes,No"), stringsAsFactors = F)
#   out <- detect.survey(df)
#   expect_is(out, "tbl_df")
#   expect_is(out$test_ma3, "list")
#   expect_equal(length(out$test_ma3), 3)
# #  expect_equal(levels(out$test_kq3), c("test_valid", "test_invalid"))
# #  expect_false(is.ordered(out$test_kq3))
#   rm(list="test_ma3", envir=mulanswer_questions)
# })
#
#

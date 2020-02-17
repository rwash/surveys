library(surveys)
context("multiple choice detector")

test_that("a multiple answer option is added correctly", {
  choices <- c("test_a", "test_b", "test_c")
  add_multiple_answer(choices)
  expect_true(exists("test_a__test_b__test_c", envir=multiple_answer_choices))
  info <- get("test_a__test_b__test_c", envir=multiple_answer_choices)
  expect_equal(info$options, choices)
  expect_false(info$case_sensitive)
})

test_that("multiple answer options are added correctly", {
  choices <- c("test_a", "test_b", "test_c", "test_d")
  add_multiple_answer(choices, case_sensitive=T)
  expect_true(exists("test_a__test_b__test_c__test_d", envir=multiple_answer_choices))
  info <- get("test_a__test_b__test_c__test_d", envir=multiple_answer_choices)
  expect_equal(info$options, choices)
  expect_true(info$case_sensitive)
})

test_that("match_ma_choiceset finds a match correctly", {
  choices <- c("test_1", "test_2")
  column <- c("test_2", "test_2", "test_1")
  column2 <- c("test_1a", "test_2", "test_1")
  column3 <- c("test_2", "test_1", "test_1,test_2")
  add_multiple_answer(choices)
  expect_equal(match_ma_choiceset(column), "test_1__test_2")
  expect_null(match_ma_choiceset(column2))
  expect_equal(match_ma_choiceset(column3), "test_1__test_2")
})

test_that("match_ma_choiceset matches with NAs", {
  choices <- c("test_11", "test_12")
  column <- c("test_12", NA, "test_11")
  column2 <- c("test_12", NA, "test_11", "test_12,test_11")
  add_multiple_answer(choices)
  expect_equal(match_ma_choiceset(column), "test_11__test_12")
  expect_equal(match_ma_choiceset(column2), "test_11__test_12")
})

test_that("multiple answer detector works", {
  choices <- c("test_111", "test_112")
  column <- c("test_112", NA, "test_111", "test_111", "test_111,test_112", "test_112")
  add_multiple_answer(choices)
  expect_true(multiple_answer_detector(column))
  expect_false(multiple_answer_detector(c("test_xyz", "test_abc")))
})

test_that("multiple answer detector works when not all options are present", {
  choices <- c("test_200", "test_201", "test_202")
  column <- c("test_200", "test_201")
  column2 <- c("test_200", "test_201", "test_200,test_201")
  add_multiple_answer(choices)
  expect_true(multiple_answer_detector(column))
  expect_true(multiple_answer_detector(column2))
})

test_that("multiple answer processor works", {
  choices <- c("test_20", "test_21")
  column <- c("test_20", NA, "test_20", "test_20", "test_20", "test_21")
  add_multiple_answer(choices)
  out <- multiple_answer_processor(column)
  expect_is(out, "tbl_df")
  expect_equal(dim(out)[2], 3) # 3 columns
  expect_equal(dim(out)[1], 6) # 6 rows
  expect_equal(out$value, purrr::map(column, ~.x))
  expect_equal(out$test_20, c(T,F,T,T,T,F))
  expect_equal(out$test_21, c(F,F,F,F,F,T))
})

test_that("MA case insensitive detector works", {
  choices <- c("test_30", "test_31")
  column <- c("TEST_30", "test_30", "Test_31", NA)
  add_multiple_answer(choices, case_sensitive=F)
  expect_true(multiple_answer_detector(column))
})

test_that("MA case sensitive detector works", {
  choices <- c("test_40", "Test_40")
  column <- c("Test_40", "test_40", "Test_40", NA)
  column2 <- c("TEST_40", "test_40", "Test_40", NA)
  add_multiple_answer(choices, case_sensitive=T)
  expect_true(multiple_answer_detector(column))
  expect_false(multiple_answer_detector(column2))
})

test_that("multiple answer comma detector works", {
  choices <- c("test_45", "test,46")
  column <- c("test_45", "test_45", "test,46", NA)
  column2 <- c("test_45", "test_45", "test_46", NA)
  add_multiple_answer(choices, case_sensitive=T)
  expect_true(multiple_answer_detector(column))
  expect_false(multiple_answer_detector(column2))
})

test_that("MA case insensitive processor works", {
  choices <- c("test_50", "Test_51")
  column <- c("TEST_50", "Test_50", "test_51", NA)
  add_multiple_answer(choices, case_sensitive=F)
  out <- multiple_answer_processor(column)
  expect_is(out, "tbl_df")
  expect_equal(dim(out), c(4,3))
  expect_equal(out$value, purrr::map(c("test_50", "test_50", "test_51", NA), ~.x))
  expect_equal(out$test_50, c(T,T,F,F))
  expect_equal(out$test_51, c(F,F,T,F))
})

test_that("multiple answer comma processor works", {
  choices <- c("test_55", "test,56")
  column <- c("test_55", "test,56", "test_55,test,56", NA)
  add_multiple_answer(choices)
  out <- multiple_answer_processor(column)
  expect_is(out, "tbl_df")
  expect_equal(dim(out), c(4,3))
  expect_equal(out$value, c("test_55", "test,56", "test_55,test,56", NA))
  expect_equal(out$test_55, c(T,F,T,F))
  expect_equal(out$test_56, c(F,T,T,F))
})
test_that("MA case sensitive processor works", {
  choices <- c("test_60", "Test_61")
  column <- c("test_60", "test_60", "Test_61", NA)
  add_multiple_answer(choices, case_sensitive=T)
  out <- multiple_answer_processor(column)
  expect_equal(dim(out), c(4,3))
  expect_equal(out$value, purrr::map(c("test_60", "test_60", "Test_61", NA), ~.x))
  expect_equal(out$test_60, c(T,T,F,F))
  expect_equal(out$Test_61, c(F,F,T,F))
})

test_that("mulitple answers processor allows multiple answers", {
  choices <- c("test_70", "test_71")
  column <- c("test_70", "test_70", "test_71", NA, "test_70,test_71")
  add_multiple_answer(choices)
  out <- multiple_answer_processor(column)
  expect_equal(dim(out), c(5,3))
  expect_equal(out$value, list("test_70", "test_70", "test_71", as.character(NA), c("test_70", "test_71")))
  expect_equal(out$test_70, c(T,T,F,F,T))
  expect_equal(out$test_71, c(F,F,T,F,T))
})

test_that("mulitple answers get detected properly in detect.question", {
  choices <- c("test_80", "test_81")
  column <- c("test_80", "test_80", "test_81", NA, "test_80,test_81")
  add_multiple_answer(choices)
  out <- detect.question(column)
  expect_equal(dim(out), c(5,3))
  expect_equal(out$value, list("test_80", "test_80", "test_81", as.character(NA), c("test_80", "test_81")))
  expect_equal(out$test_80, c(T,T,F,F,T))
  expect_equal(out$test_81, c(F,F,T,F,T))
})

test_that("mulitple answers get detected properly in detect.survey", {
  choices <- c("test_90", "test_91")
  column <- c("test_90", "test_90", "test_91", NA, "test_90,test_91")
  add_multiple_answer(choices)
  survey <- data.frame(id=1:length(column), col=column, other=length(column):1)
  out <- detect.survey(survey)
  expect_equal(dim(out), c(5,5))
  expect_equal(out$col_value, list("test_90", "test_90", "test_91", as.character(NA), c("test_90", "test_91")))
  expect_equal(out$col_test_90, c(T,T,F,F,T))
  expect_equal(out$col_test_91, c(F,F,T,F,T))
})


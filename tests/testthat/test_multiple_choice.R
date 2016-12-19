library(surveys)
context("multiple choice detector")

test_that("a multiple choice option is added correctly", {
  choices <- c("test_a", "test_b", "test_c")
  add_multiple_choice(choices)
  expect_true(exists("test_a__test_b__test_c", envir=surveys::multiple_choices))
  info <- get("test_a__test_b__test_c", envir=surveys::multiple_choices)
  expect_equal(info$options, choices)
  expect_false(info$case_sensitive)
  expect_true(info$ordered)
})

test_that("multiple choice options are added correctly", {
  choices <- c("test_a", "test_b", "test_c", "test_d")
  add_multiple_choice(choices, case_sensitive=T, ordered=F)
  expect_true(exists("test_a__test_b__test_c__test_d", envir=surveys::multiple_choices))
  info <- get("test_a__test_b__test_c__test_d", envir=surveys::multiple_choices)
  expect_equal(info$options, choices)
  expect_true(info$case_sensitive)
  expect_false(info$ordered)
})

test_that("match_choiceset finds a match correctly", {
  choices <- c("test_1", "test_2")
  column <- c("test_2", "test_2", "test_1")
  column2 <- c("test_1a", "test_2", "test_1")
  add_multiple_choice(choices)
  expect_equal(match_choiceset(column), "test_1__test_2")
  expect_null(match_choiceset(column2))
})

test_that("match_choiceset matches with NAs", {
  choices <- c("test_11", "test_12")
  column <- c("test_12", NA, "test_11")
  add_multiple_choice(choices)
  expect_equal(match_choiceset(column), "test_11__test_12")
})

test_that("multiple choice detector works", {
  choices <- c("test_111", "test_112")
  column <- c("test_112", NA, "test_111", "test_111", "test_111", "test_112")
  add_multiple_choice(choices)
  expect_true(multiple_choice_detector(column))
  expect_false(multiple_choice_detector(c("test_xyz", "test_abc")))
})

test_that("multiple choice detector works when not all options are present", {
  choices <- c("test_200", "test_201", "test_202")
  column <- c("test_200", "test_201")
  add_multiple_choice(choices)
  expect_true(multiple_choice_detector(column))
})

test_that("multiple choice processor works", {
  choices <- c("test_20", "test_21")
  column <- c("test_20", NA, "test_20", "test_20", "test_20", "test_21")
  add_multiple_choice(choices)
  out <- multiple_choice_processor(column)
  expect_is(out, "factor")
  expect_is(out, "ordered")
  expect_equal(levels(out), c("test_20", "test_21"))
  expect_equal(as.numeric(out), c(1, NA, 1, 1, 1, 2))
})

test_that("case insensitive detector works", {
  choices <- c("test_30", "test_31")
  column <- c("TEST_30", "test_30", "Test_31", NA)
  add_multiple_choice(choices, case_sensitive=F)
  expect_true(multiple_choice_detector(column))
})

test_that("case sensitive detector works", {
  choices <- c("test_40", "Test_40")
  column <- c("Test_40", "test_40", "Test_40", NA)
  column2 <- c("TEST_40", "test_40", "Test_40", NA)
  add_multiple_choice(choices, case_sensitive=T)
  expect_true(multiple_choice_detector(column))
  expect_false(multiple_choice_detector(column2))
})

test_that("case insensitive processor works", {
  choices <- c("test_50", "Test_51")
  column <- c("TEST_50", "Test_50", "test_51", NA)
  add_multiple_choice(choices, case_sensitive=F)
  out <- multiple_choice_processor(column)
  expect_is(out, "factor")
  expect_is(out, "ordered")
  expect_equal(levels(out), c("test_50", "Test_51"))
  expect_equal(as.numeric(out), c(1, 1, 2, NA))
})

test_that("case sensitive processor works", {
  choices <- c("test_60", "Test_61")
  column <- c("test_60", "test_60", "Test_61", NA)
  add_multiple_choice(choices, case_sensitive=T)
  out <- multiple_choice_processor(column)
  expect_is(out, "factor")
  expect_is(out, "ordered")
  expect_equal(levels(out), c("test_60", "Test_61"))
  expect_equal(as.numeric(out), c(1, 1, 2, NA))
})

test_that("ordered processor works", {
  choices <- c("test_71", "test_70")
  column <- c("test_70", "test_70", "test_71", NA)
  add_multiple_choice(choices, ordered=T)
  out <- multiple_choice_processor(column)
  expect_is(out, "factor")
  expect_is(out, "ordered")
  expect_equal(levels(out), c("test_71", "test_70"))
  expect_equal(as.numeric(out), c(2, 2, 1, NA))
})

# Note: it preserves the order of the levels specified anyway
test_that("unordered processor works", {
  choices <- c("test_81", "test_80")
  column <- c("test_80", "test_80", "test_81", NA)
  add_multiple_choice(choices, ordered=F)
  out <- multiple_choice_processor(column)
  expect_is(out, "factor")
  expect_false(is.ordered(out))
  expect_equal(levels(out), c("test_81", "test_80"))
  expect_equal(as.numeric(out), c(2, 2, 1, NA))
})

test_that("agreement levels are detected", {
  column <- c("Agree", "Disagree")
  expect_true(multiple_choice_detector(column))
  column2 <- c("strongly disagree", "agree", "neither agree nor disagree", "strongly disagree")
  expect_true(multiple_choice_detector(column))
})

test_that("agreement levels are processed properly", {
  column <- c("Agree", "Disagree")
  out <- multiple_choice_processor(column)
  expect_is(out, "ordered")
  expect_equal(as.numeric(out), c(4, 2))
  column2 <- c("strongly disagree", "agree", "neither agree nor disagree", "strongly disagree")
  out2 <- multiple_choice_processor(column2)
  expect_is(out2, "ordered")
  expect_equal(as.numeric(out2), c(1, 4, 3, 1))
})

test_that("detect question properly processes agree/disagree scales", {
  column <- c("Agree", "Disagree", "neither Agree nor Disagree", "Strongly Agree", "Agree")
  out <- detect.question(column)
  expect_is(out, "ordered")
  expect_equal(as.numeric(out), c(4, 2, 3, 5, 4))
})

test_that("detect question properly with NAs", {
  column <- c("Agree", "Disagree", "neither Agree nor Disagree", NA, "Strongly Agree", "Agree")
  out <- detect.question(column)
  expect_is(out, "ordered")
  expect_equal(as.numeric(out), c(4, 2, 3, NA, 5, 4))
  expect_equal(levels(out), c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))
})

test_that("detect question properly with factor input", {
  column <- factor(c("Agree", "Disagree", "neither Agree nor Disagree", "Strongly Agree", "Agree"))
  out <- detect.question(column)
  expect_is(out, "ordered")
  expect_equal(as.numeric(out), c(4, 2, 3, 5, 4))
  expect_equal(levels(out), c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))
})

test_that("detect question properly with NAs in factor input", {
  column <- factor(c("Agree", "Disagree", "neither Agree nor Disagree", NA, "Strongly Agree", "Agree"))
  out <- detect.question(column)
  expect_is(out, "ordered")
  expect_equal(as.numeric(out), c(4, 2, 3, NA, 5, 4))
  expect_equal(levels(out), c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"))
})


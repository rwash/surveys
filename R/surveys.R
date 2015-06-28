# ***************************************
# ** Functions to make cleaning surveys easier
# ***************************************

#' Apply a function to specific columns
#'
#' Apply a function to a specified set of columns in a data frame
#'
#' @param df The data.frame to apply to
#' @param fun The function to apply.  The first parameter should be the column of data
#' @param cols The set of columns to apply to
#' @param ... Additional parameters to pass into \code{fun}
#'
#' @return A new copy of the data frame with the modified columns
#' @export
#'
#' @examples
#' df <- data.frame(one=c("1.0", "2.0"), two=c("3.0", "4.0"))
#' df <- apply_columns(df, as.numeric, c("one", "two"))
apply_columns <- function(df, fun, cols, ...) {
  for (col in cols) {
    df[[col]] <- fun(df[[col]], ...)
  }
  df
}

#' Add numeric versions of columns
#'
#' Take a data frame and add numeric versions of specified factor columns
#'
#' @param df The data.frame with the source data
#' @param cols A list of column names to convert
#' @param fun The function to apply to all of the columns
#' @param ... Additional parameters to fun
#'
#' @return A new data frame with additional columns.  Each column listed in \code{cols} will have
#'   a new version added, column\code{_n}, containing the data data as in column convert to numeric
#' @export
make_columns_numeric <- function(df, cols, fun=as.numeric, ...) {
  for (col in cols) {
    df[[paste(col, "_n", sep="")]] <- fun(df[[col]], ...)
  }
  df
}

#' Create all combinations of sets of strings
#'
#' Combine two sets of strings to create all possible combinations
#'
#' @param ... One or more character vectors
#' @param sep String to use as a separator character
#'
#' @return A vector of all possible combinations of the input paramters
#' @export
#'
#' @examples
#' combine_names(c("is", "as"), c("numeric", "logical"))
combine_names <- function(..., sep=".") {
  nargs <- length(args <- list(...))
  df <- expand.grid(args)
  chs <- apply(df, 2, as.character)
  if (length(df[,1]) > 1) {
    chs <- apply(chs, 2, stringr::str_trim)
  } else {
    chs <- trim(chs)
  }
  if (class(chs) == "character") {
    paste(chs, collapse=sep)
  } else {
    apply(chs, 1, paste, collapse=sep)
  }
}

#' Create a formula from a list of variable names
#'
#' Creates a formula from a list of variable names.  If more than one list of names, it combines
#' them using \code{\link{combine_names}} to create all combinations of names.
#'
#' @param ... One or more lists of variable names to make into a formula
#' @param type Combination method.  Defaults to '+'.  It might also be useful to do '*'
#'
#' @return A formula containing all of the specified names
#' @export
#'
#' @examples
#' mf(c("var"), c("one", "two"), "_n")
mf <- function(..., type="+") {
  temp <- combine_names(...)
  t2 <- paste("~ ", paste(temp, collapse=type))
  formula(t2)
}


# Default names for levels for different types of questions.  Can be overridden
familiarity_levels=c("Not Familiar", "Slightly Familiar", "Somewhat Familiar", "Familiar", "Very Familiar")
#familiarity_levels=c("None", "Little", "Some", "Good", "Full")
emotion_levels=c("Annoyed", "Comfortable", "Confused", "Curious", "Fine", "In control", "Nervous", "Scared", "Useless", "Not concerned", "It has never  happen to me", "This no longer happens to me")
comfort_levels=c("Very Uncomfortable", "Uncomfortable", "Undecided", "Comfortable", "Very Comfortable")
agree_levels=c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
concern_levels=c("Very Unconcerned", "Unconcerned", "Undecided", "Concerned", "Very Concerned")
frequency_levels=c("Never", "Rarely", "Sometimes", "Often", "Always")
yesno_levels=c("No", "Yes")
likely_levels=c("Very Unlikely", "Unlikely", "Undecided", "Likely", "Very Likely")
education_levels=c("None, or grades 1-8",
                   "High school incomplete (9-11)",
                   "High school graduate (grade 12 or GED certificate)",
                   "Technical, trade, or vocational school AFTER high school",
                   "Some college, not 4-year degree (includes Associates Degree)",
                   "College graduate (B.S., B.A., or other 4-year degree)",
                   "Post-graduate training/professional school after college (toward a Masters Degree/ PhD, Law, or Medical school)",
                   "Post-graduate degree (Masters/PhD, Law, or Medical school)")
race_levels=c("American Indian or Alaska Native", "Asian or Pacific Islander",
              "Black or African American", "Hispanic or Latino", "White",
              "Other (please specify):")
gender_levels=c("Man", "Woman")


# Function that processes a survey question: converts to the appropriate type and renames it
question <- function(survey, col, new_name=col, question_type=checkbox, ...) {
  survey[[col]] <- question_type(survey[[col]], ...)
  names(survey)[names(survey) == col] <- new_name # Rename column
  survey
}





# TODO: Write a column type auto-detectors
# http://www.r-bloggers.com/package-wide-variablescache-in-r-packages/

question_types = new.env(parent=emptyenv())

#' New question type.
#'
#' Adds a new type of question to the survey question auto-detector.
#'
#' @param name Text string containing the name of this question type
#' @param detector Function that takes a single argument -- a vector of data -- and
#'   returns True or False -- whether this data is of the correct type
#' @param processor Function that takes a single argument -- a vector of data --
#'   processes it, and returns the processed data
add_question_type <- function(name, detector, processor) {
  if (exists(name, envir=question_types)) {
    warning("Question Type already known")
  }
  assign(name, list(detector, processor), envir=question_types)
}


#' Question Auto-Detect
#'
#' Automatically detect what type of question this column of data came from, and process it
#'
#' @param column A single vector of data from a survey
#'
#' @return A vector of processed data
detect.question <- function(column) {
  types <- ls(envir=question_types)
  for (type in types) {
    qtype <- get(type, envir=question_types)
    detector <- qtype[[1]]
    if (detector(column)) {
      # cat(paste("Found", type, "\n"))
      processor <- qtype[[2]]
      return(processor(column))
    }
  }
  warning("Column of unknown type")
  return(column)
}

#' Survey Cleaning Auto-detect
#'
#' Automatically clean a survey by iterating through a data frame and automatically detecting
#' each question's type and processing it
#'
#' @param frame The data.frame containing the survey
#'
#' @return A new data.frame containing the processed survey
detect.survey <- function(frame) {
  out <- lapply(frame, detect.question)
  out <- as.data.frame(out, stringsAsFactors=F)
  return(out)
}

#' Load a survey dataset from a file
#'
#' Loads a survey dataset from a file, auto-detects all of the columns, and then processes the
#' dataset
#'
#' @param file Name of the file to load
#'
#' @return A data.frame containing the processed dataset
load_survey <- function(file, ...) {
  f <- read.csv(file, stringsAsFactors=F, ...)
  return(detect.survey(f))
}


# TODO: Should I add a priority ordering?

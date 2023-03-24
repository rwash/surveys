
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
#' @export
add_question_type <- function(name, detector, processor) {
  if (exists(name, envir=question_types)) {
    warning("Question Type already known")
  }
  detector <- match.fun(detector)
  if (is.character(processor) && stringr::str_to_lower(processor) == "remove") {
    processor <- "remove"
  } else {
    processor <- match.fun(processor)
  }
  assign(name, list(detector, processor), envir=question_types)
}

#' Remove a question type from the auto-detector
#'
#' @param name Character string name of the question type to remove
#' @export
remove_question_type <- function(name) {
  if (exists(name, envir=question_types))
    rm(list=name, envir=question_types)
}

replace_nas <- function(v) {
  if (is.factor(v)) {
    l <- levels(v)
    l <- ifelse(l=="", NA, l)
    levels(v) <- l
    return(v)
  }
  return(ifelse(v=="", NA, v))
}


#' Question Auto-Detect
#'
#' Automatically detect what type of question this column of data came from, and process it
#'
#' @param column A single vector of data from a survey
#' @param col_name (optional) The name of the column
#'
#' @return A vector of processed data
#' @export
detect.question <- function(column, col_name="") {
  column <- replace_nas(column)
  attr(column, "name") <- col_name
  types <- ls(envir=question_types)
  for (type in types) {
    qtype <- get(type, envir=question_types)
    detector <- qtype[[1]]
    if (detector(column)) {
      # cat(paste("Found", type, "\n"))
      processor <- qtype[[2]]
      if (is.character(processor) && processor == "remove") {
        return(NULL)
      } else {
        return(processor(column))
      }
    }
  }
  warn_msg <- "Column of unknown type"
  if (col_name != "") { warn_msg <- paste0("Column ", col_name, " of unknown type")}
  warning(warn_msg)
  attr(column, "name") <- NULL
  return(column)
}

#' Survey Cleaning Auto-detect
#'
#' Automatically clean a survey by iterating through a data frame and automatically detecting
#' each question's type and processing it
#'
#' @param frame The data.frame or tibble containing the survey
#'
#' @return A new tibble containing the processed survey
#' @export
detect.survey <- function(frame) {
  n_rows <- dim(frame)[1]
  out <- lapply(names(frame), function(n) { detect.question(frame[[n]], n)} )
  names(out) <- names(frame)
  for (i in names(out)) {
    if (is.null(out[[i]])) {
      out[[i]] <- NULL  # Actually remove the column if it is all NULL
    }
  }
  out_tibble <- tibble::tibble(.rows = n_rows)
  for (i in names(out)) {
    if (tibble::is_tibble(out[[i]])) {
      names(out[[i]]) <- stringr::str_c(i, "_", names(out[[i]]))
      out_tibble <- dplyr::bind_cols(out_tibble, out[[i]])
    } else {
      out_tibble <- tibble::add_column(out_tibble, !!i := out[[i]])
    }
  }
  # name_fix <- function(x) {stringr::str_remove(x, "\\$value")}
  # out <- tibble::as_tibble(out, .name_repair = ~name_fix)
  return(out_tibble)
}

#' Load a survey dataset from a file
#'
#' Loads a survey dataset from a file, auto-detects all of the columns, and then processes the
#' dataset
#'
#' @param file Name of the file to load
#'
#' @return A tibble containing the processed dataset
#' @export
load_survey <- function(file, ...) {
  f <- readr::read_csv(file, ...)
  attr(f, "question.text") <- f[1,]
  f <- f[-1:-2,]
  return(detect.survey(f))
}


#' Resets the surveys package
#'
#' Removes all known question types and answer types, and resets the surveys packag
#' back to where it is when it is first loaded
#'
#' @export
reset_surveys <- function() {
  rm(list=ls(envir=question_types), envir=question_types)
  rm(list=ls(envir=multiple_choices), envir=multiple_choices)
  rm(list=ls(envir=multiple_answer_choices), envir=multiple_answer_choices)
  rm(list=ls(envir=attention_checks), envir=attention_checks)
  rm(list=ls(envir=ignore_questions), envir=ignore_questions)
  rm(list=ls(envir=known_questions), envir=known_questions)

  load_question_types()
  load_multiple_choice_options()
}

# TODO: Should I add a priority ordering?

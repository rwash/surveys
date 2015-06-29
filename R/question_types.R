
# Helper functions; mostly detector functions
char_is_numeric <- function(x) { return(all(grepl("^[-+]?[1-90]+(\\.[1-90]*)?$", x) | is.na(x))) }
char_is_logical <- function(x) { return(all(grepl("^(false)$|^(true)$|^f$|^t$|^0$|^1$", x, ignore.case=T) | is.na(x)))}
char_is_checkbox <- function(x) { return(F)} # How is checkbox different from logical?s
char_is_ip <- function(x) { return(all(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$", x) | is.na(x))) }
is_qualtrics_subject_id <- function(x) { return(all(grepl("^R_[a-zA-Z0-9]{15}$", x) | is.na(x)))}
all_identical <- function(x) { return(length(unique(x)) == 1 || all(is.na(x)))}
char_is_date <- function(x) { return(all(!is.na(suppressWarnings(lubridate::ymd_hms(x))))) }

# Convert 0/1 to T/F.  Useful when converting a logical question
convert_logical <- function(x) { return(sub("0", "F", sub("1", "T", x))) }

# A checkbox has a text value (for checked), or NA (for unchecked)
char_is_checkbox <- function(x) { return(length(unique(x)) == 2 && length(na.omit(unique(x))) == 1) }
checkbox <- function(x) { return(!is.na(x)) }

#checkbox <- function(x) { sapply(x, isTRUE) } # How is checkbox different from logical?

# --- Attention Check Questions ---
attention_checks <- new.env(parent=emptyenv())

#' Attention Check questions
#'
#' Tells the question auto-detection system that a specific question is an attention check question
#'
#' Attention check questions are questions whose goal is to determine whether the subject is
#' paying close attention to questions.  It has exactly one right answer, and any other answer
#' is invalid.
#'
#' @param q_name The column name of the attention check question
#' @param valid What value is to be considered valid
#'
#' @export
add_attention_check <- function(q_name, valid) {
  if (exists(q_name, envir=attention_checks)) {
    warning(paste0(q_name, " is already an attention check.  Overwriting..."))
  }
  assign(q_name, valid, envir=attention_checks)
}

is_attention_check <- function(column) {
  attention_qs <- ls(envir=attention_checks)
  return(attr(column, "name") %in% attention_qs)
}

attention_check <- function(column) {
  valid_value <- get(attr(column, "name"), envir=attention_checks)
  return(column == valid_value & !is.na(column))
}

# --- Ignore questions ---
ignore_questions <- new.env(parent=emptyenv())

#' Ignore a specific question
#'
#' Tells the question auto-detector to ignore a specific question.  This doesn't do any processing
#' just passes the raw character vector through
#'
#' @param q_name The name of the question to ignore
#' @export
ignore_question <- function(q_name) {
  if (exists(q_name, envir=attention_checks)) {
    warning(paste0(q_name, " is already being ignored..."))
  }
  assign(q_name, "ignore", envir=ignore_questions)
}

is_ignore_question <- function(column) {
  ignore_qs <- ls(envir=ignore_questions)
  return(attr(column, "name") %in% ignore_qs)
}

load_question_types <- function() {
  add_question_type("ignored", is_ignore_question, as.character)
  add_question_type("attention.check", is_attention_check, attention_check)
  add_question_type("numeric", char_is_numeric, as.numeric)
  add_question_type("logical", char_is_logical, function(x) { as.logical(convert_logical(x)) } )
  add_question_type("ip.address", char_is_ip, as.character)
  add_question_type("qualtrics.subject_id", is_qualtrics_subject_id, as.character)
  add_question_type("no_variation", all_identical, "remove")
  add_question_type("date", char_is_date, lubridate::ymd_hms)
  add_question_type("checkbox", char_is_checkbox, checkbox)
}

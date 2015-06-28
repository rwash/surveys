multiple_choices = new.env(parent=emptyenv())


#' Add a new set of multiple choice options
#'
#' @param choice_set Character vector of options
#' @param ordered Are the options in a particular order?
#' @param case_sensitive Are the options case sensitive?
#' @export
add_multiple_choice <- function(choice_set, ordered=T, case_sensitive=F) {
  if (exists(choice_set, envir=multiple_choices)) {
    warning("Multiple Choice set already exists")
  }
  name <- paste0(choice_set, collapse="__")
  assign(name, list(options=choice_set, ordered=ordered, case_sensitive=case_sensitive), envir=multiple_choices)
}

match_choiceset <- function(column) {
  choice_sets <- ls(envir=multiple_choices)
  for(set in choice_sets) {
    set_info <- get(set, envir=multiple_choices)
    opts <- sort(set_info$options)
    values <- column
    if (!set_info$case_sensitive) {
      opts <- stringr::str_to_lower(opts)
      values <- stringr::str_to_lower(values)
    }
    values <- sort(unique(values))
    if (all(values %in% opts)) {
      return(set)
    }
  }
  return(NULL)

}

multiple_choice_detector <- function(column) {
  return(!is.null(match_choiceset(column)))
}

multiple_choice_processor <- function(column) {
  set <- match_choiceset(column)
  if (is.null(set)) { return(column) }
  set_info <- get(set, envir=multiple_choices)
  choices <- set_info$options
  if (!set_info$case_sensitive) {
    column <- stringr::str_to_lower(column)
    choices <- stringr::str_to_lower(choices)
  }
  out <- factor(column, levels=choices, labels=set_info$options, ordered=set_info$ordered)
  return(out)
}



load_multiple_choice_options <- function() {
  add_question_type("multiple_choice", multiple_choice_detector, multiple_choice_processor)
  # Add some standard multiple choice option sets
  add_multiple_choice(c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"), case_sensitive=F, ordered=T)
  add_multiple_choice(c("Never", "Rarely", "Sometimes", "Often", "Always"))
  add_multiple_choice(c("No", "Yes"))
  add_multiple_choice(c("Man", "Woman"), ordered=F)
  add_multiple_choice(c("Male", "Female"), ordered=F)
}


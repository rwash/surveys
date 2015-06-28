multiple_choices = new.env(parent=emptyenv())


#' Add a new set of multiple choice options
#'
#' @param choice_set Character vector of options
#' @param ordered Are the options in a particular order?
#' @param case_sensitive Are the options case sensitive?
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
  add_multiple_choice(c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"), case_sensitive=F, ordered=T)
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

multiple_answer_choices = new.env(parent=emptyenv())


#' Add a new set of multiple answer choice options.  Mulitple answers have checkboxes that
#' allow the survey participant to choose more than one answer.
#'
#' @param choice_set Character vector of options
#' @param case_sensitive Are the options case sensitive?
#' @export
add_multiple_answer <- function(choice_set, case_sensitive=F) {
  if (exists(choice_set, envir=multiple_answer_choices)) {
    warning("Multiple Choice set already exists")
  }
  name <- paste0(choice_set, collapse="__")
  assign(name, list(options=choice_set, case_sensitive=case_sensitive), envir=multiple_answer_choices)
}

match_ma_choiceset <- function(column) {
  choice_sets <- ls(envir=multiple_answer_choices)
  for(set in choice_sets) {
    set_info <- get(set, envir=multiple_answer_choices)
    opts <- sort(set_info$options)
    # Multiple answers are comma separated; let's pull them out
    values <- unlist(stringr::str_split(column, ","))
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

multiple_answer_detector <- function(column) {
  return(!is.null(match_ma_choiceset(column)))
}

multiple_answer_processor <- function(column) {
  set <- match_ma_choiceset(column)
  if (is.null(set)) { return(column) }
  set_info <- get(set, envir=multiple_answer_choices)
  choices <- set_info$options
  if (!set_info$case_sensitive) {
    column <- stringr::str_to_lower(column)
    choices <- stringr::str_to_lower(choices)
  }
  out <- tibble::enframe(stringr::str_split(column, ","), name=NULL)
  mutate_opts <- purrr::map(choices, ~rlang::quo(purrr::map_lgl(value, ~!!.x %in% .x)))
  names(mutate_opts) <- choices
  out <- out %>% dplyr::mutate(!!! mutate_opts)
  return(out)
}




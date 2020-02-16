
#' @importFrom rlang .data

.onLoad <- function(libname, pkgname) {
  load_question_types()
  load_multiple_choice_options()
  invisible()
}

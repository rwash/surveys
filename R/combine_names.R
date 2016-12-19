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
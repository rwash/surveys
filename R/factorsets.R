#source("multiplot.R")

cutoff <- function(x, cutoff=0.4, dec=T) {
  sort(x[abs(x) > cutoff], dec=dec)
}

#' Identify a set of candidate questions for new factors.
#'
#' Use maximum likelihood factor analysis to identify a set of candidate questions for new factors.
#'
#' Uses \code{factanal} to conduct a factor analysis of a large number of questions.  Then identifies the
#' top Q questions in each of F factors.  It reruns a new factor analysis on just those questions to ensure
#' they still hold together
#'
#' @param scale An \code{item.scale} object that specifies the set of questions to be analyzed
#' @param factors The number of factors to extract
#' @param questions The maximum number of questions to identify in each factor.
#' @param cut The minimum factor loading for a question to be considered part of a factor
#' @return A \code{factorset} object
#' @export
factorset <- function(scale, factors=4, questions=5, cut=0.4) {
  fa <- factanal(scale, factors=factors)
  qs <- lapply(1:factors, function(i) { cutoff(loadings(fa)[,i], cutoff=cut)[1:questions] })
  factor.names <- lapply(qs, function(x) { names(x)[!is.na(names(x))] })
  allnames <- unlist(factor.names)
  allnames <- allnames[!is.na(allnames)]
  separate_scales <- lapply(1:factors, function(i) {
    n <- factor.names[[i]][!is.na(factor.names[[i]])]
    item.scale(mf(n), data=attr(scale, "model.frame.orig"), numeric=attr(scale, "numeric"))
  })
  new_scale <- item.scale(mf(allnames), data=attr(scale, "model.frame"))
  new_fa <- factanal(new_scale, factors=factors)

  attr(qs, "factor.qs") <- factor.names
  attr(qs, "all.names") <- allnames
  attr(qs, "scale") <- new_scale
  attr(qs, "factanal") <- new_fa
  attr(qs, "item.scales") <- separate_scales
  attr(qs, "factors") <- factors
  attr(qs, "questions") <- questions
  attr(qs, "cutoff") <- cut
  attr(qs, "class") <- c("factorset", class(qs))
  qs
}

#' Print a summary of a factorset
#'
#' Prints out the final factor analysis of a factorset
#'
#' @param fs The \code{factorset} object to be printed
#' @param cut (optional) The cutoff for \code{print.factanal}; Only prints factor loading above this value
#' @export
print.factorset <- function(fs, cut=NULL) {
  # First, print out the list of questions for each factor (concisely)
  for (i in 1:attr(fs, "factors")) {
    cat(i, ": ", paste(attr(fs, "factor.qs")[[i]], sep=":", collapse=", "), "\n")
  }
  df <- data.frame(attr(fs, "item.scales"))
  names(df) <- combine_names("factor", 1:attr(fs, "factors"))
  print(summary(df))
  # Next, print out the factor analysis of just these questions
  if (is.null(cut)) { cut <- attr(fs, "cutoff")}
  print(attr(fs, "factanal"), cutoff=cut)
}

#' Draw a plot of a factorset
#'
#' Draws a set of histograms where each question has its own histogram, and each factor is a row
#'
#' @param fs A \code{factorset} object
#' @param ymax The maximum y value to draw.  This ensures that all items are visually comparable.
plot.factorset <- function(fs, ymax=1150) {
  scales <- attr(fs, "item.scales")
  plots <- lapply(scales, function(s) { plot_list(s, ymax=ymax)})
  plots <- unlist(plots, F)
  q <- attr(fs, "questions")
  f <- attr(fs, "factors")
  multiplot(plotlist=plots, layout=matrix(1:(q*f), ncol=q, byrow=T))
}

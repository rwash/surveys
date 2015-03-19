
#' Generate a histogram of the responses to a single survey question
#'
#' Using ggplot2, generate a plot object that will display a histogram of the responses to
#' single survey item.
#'
#' @param d A data frame containing the source data
#' @param question Which column in \code{d} to plot
#' @param labels an optional list of labels for the question. This is usually the list of
#'   question options from the survey
#' @param ymax The maximum y value (ylim) for the plot.  This is used to ensure that a set of plots are comparable.
#' @return A ggplot2 plot object containing a histogram
plot_question <- function(d, question, labels=NULL, ymax=1200) {
  d <- subset(d, !is.na(d[[question]]))
  out <- ggplot(d, aes_string(x=question)) + geom_histogram(binwidth=1) +
    theme_bw() + scale_x_discrete(drop=F) + ylim(0,ymax)
  if (!is.null(labels)) {
    out <- out + scale_x_discrete(labels=labels)
  }
  out
}

#' Generate a series of histograms for all of the questions in an \code{item.scale}
#'
#' This will iteratively generate a list of histogram plots, one for each of the questions listed in an \code{item.scale}
#'
#' @param s An \code{item.scale} object
#' @param ... Any additional parameters for \code{plot_question}
#' @return A \code{list} of ggplot2 objects
plot_list <- function(s, ...) {
  mf <- attr(s, "model.frame.orig")
  plots <- lapply(colnames(mf), function(n) { plot_question(mf, n, ...)})
  plots
}

#' Generate a set of histograms for the items in an \code{item.scale}
#'
#' Extracts the original data from an \code{item.scale}, generates a histogram for each question, and
#' plots them side-by-side
#'
#' @param s An \code{item.scale} object
#' @param cols (optional) The number of columns in the resulting plot.  If omitted, the function will make its best guess
#' @param ... Additional parameters passed to \code{plot_question}.  Commonly used to set a \code{ymax} value
#'   so all plots have the same y range
#' @return A grid object containing the set of histograph ggplot2 plots
#' @export
plot.item.scale <- function(s, cols=NULL, ...) {
  plots <- plot_list(s, ...)
  if (is.null(cols)) {
    mf <- attr(s, "model.frame")
    cols <- floor(sqrt(dim(mf)[2]))
  }
  multiplot(plotlist=plots, cols=cols)
}

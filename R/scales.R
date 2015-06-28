# ***************************************
# ** Functions to form items into a scale and analyze scales
# ***************************************

#' Function to standardize an item or scale.
#'
#' Standardizes a vector or column of data by centering around the mean and making
#'   standard deviation equal to one
#'
#' Note: This function automatically removes NAs when calculating the mean and standard deviation
#'
#' @param x A vector of data values to be standardized
#' @return Standardized data values
#'
#' @export
#' @examples
#' ex <- standardize(rnorm(100, 10, 5))
#' mean(ex)
#' sd(ex)
#'
standardize <- function(x) {
  mean = mean(x, na.rm=T)
  sd = sd(x, na.rm=T)
  (x - mean) / sd
}

#' Alternative way of calculating cronbach's alpha
#'
#' Cronbach's alpha is a measure of internal reliability.
#'
#' This is a slightly different way to calculate it than what is in the psy package.
#'
#' Note: Data is run through na.omit to remove NAs when calculating alpha.
#'
#' @param v1 A matrix or data frame where each column is an item, and each row is a data point.
#' @return A list containing three items: sample.size, number.of.items, and alpha
#' @export
alt_alpha <- function(v1) {
  v1 <- na.omit(v1)
  nv1 <- ncol(v1)
  pv1 <- nrow(v1)
  #  alpha <- (nv1/(nv1 - 1)) * (1 - sum(apply(v1, 2, var))/var(apply(v1, 1, sum)))
  ctemp <- cov(v1)
  vbar <- mean(diag(ctemp))
  diag(ctemp) <- NA
  cbar <- mean(ctemp, na.rm=T)
  alpha <- (nv1 * cbar) / (vbar + (nv1 - 1)*cbar)
  resu <- list(sample.size = pv1, number.of.items = nv1, alpha = alpha)
  resu
}

#' Functions to form a scale from a set of survey items
#'
#' Combines multiple items, specified using a formula interface, into a single "scale"
#'
#' @param formula A formula specifying the items to combine into a scale
#'   Should be of the form \code{~ item1 + item2 + item3}
#' @param data The data frame to use to get the data from
#' @param subset Specifies if only a specific subset of the data should be used
#' @param na.action What should be done with NAs?
#' @param drop.unused.level If factors are being combined, should we only include levels that
#'   are actually used?
#' @param scale.fun which function to compose the scale.   examples include sum, mean
#' @param standard should the individual items be standardized before inclusion in the scale?
#' @param numeric should the individual items be cast into numeric before inclusion in the scale?
#' @param ... currently unused
#' @return An object of type \code{item.scale}.  This is fundamentally a vector, and can
#'   be used as such.  It has a number of additional attributes (such as \code{cronbach})
#'   that are useful in future calculations and functions
#' @export
item.scale <- function(formula, data=NULL, subset=NULL, na.action = na.pass,
                       drop.unused.levels=F, scale.fun=mean, standard=FALSE,
                       numeric=FALSE, ...) {
  # Extract the appropriate variables using model.frame
  # This is roundabout -- form the call and then evaluate it.
  mfc <- match.call(expand.dots=FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "drop.unused.levels"), names(mfc), 0L)
  mfc <- mfc[c(1,m)]
  mfc[["na.action"]] <- na.action
  mfc[["drop.unused.levels"]] <- drop.unused.levels
  mfc[[1]] <- as.name("model.frame")
  mf <- eval(mfc, parent.frame())
  item.labels <- levels(mf[,1])
  mf.orig <- mf
  # Construct the scale
  if (standard) {
    mf <- apply(mf, 2, standardize)
  }
  if (numeric) {
    mf <- as.data.frame(lapply(mf, as.numeric))
  }
  scale <- apply(mf, 1, scale.fun, na.rm=F)
  alpha <- cronbach(mf)$alpha
  altalpha <- alt_alpha(mf)$alpha
  item.cor <- cor(mf, use="complete.obs")

  attr(scale, "cronbach") <- alpha
  attr(scale, "altalpha") <- altalpha
  attr(scale, "model.frame") <- mf
  attr(scale, "formula") <- formula
  attr(scale, "call") <- match.call()
  attr(scale, "standardized") <- standard
  attr(scale, "item.cor") <- item.cor
  attr(scale, "labels") <- item.labels
  attr(scale, "model.frame.orig") <- mf.orig
  attr(scale, "numeric") <- numeric
  class(scale) <- c("item.scale", "numeric")
  scale
}

#' Iteratively drop items from a scale and recompute scale measures
#'
#' This allows you to take an item.scale and iteratively drop items.  It recalculates the scale and
#' also calculates new Cronbach's alpha for each new scale.
#'
#' This is useful during exploratory analysis in determining whether all items are needed in a scale.
#' Each item in \code{scope} will be dropped separately in the order they are given and a
#' new scale/alpha will be computed.
#'
#' If scope is omitted, all items in the scale will be iteratively dropped.
#'
#' @param x An \code{item.scale}
#' @param scope A character vector containing the list of items that will be dropped.
#' @return An \code{n} by 3 matrix where \code{n} is the number of items to be dropped.
#'   Includes the mean, standard deviation, and alpha of each new scale as items are dropped.
#' @export
drop1.item.scale <- function(x, scope) {
  if (missing(scope)) {
    if ("terms" %in% names(attributes(attr(x, "model.frame")))) {
      scope <- drop.scope(attr(attr(x, "model.frame"),"terms"))
    } else {
      scope <- attr(attr(x, "model.frame"), "names")
    }
  }
  if (!is.character(scope)) {
    stop("Scope must be a character vector")
  }
  scale <- attributes(x)
  scale$data <- x
  ni <- length(scope)
  ans <- matrix(nrow=ni+1, ncol=3, dimnames=list(c("<none>",scope), c("Mean", "SD", "Alpha")))
  ans[1,1] <- mean(x, na.rm=T)
  ans[1,2] <- sd(x,na.rm=T)
  ans[1,3] <- attr(x, "cronbach")
  for (i in seq(ni)) {
    term <- scope[i]
    newfit <- update(scale, as.formula(paste("~ . - ", term)))
    ans[i+1,1] <- mean(newfit, na.rm=T)
    ans[i+1,2] <- sd(newfit,na.rm=T)
    ans[i+1,3] <- attr(newfit, "cronbach")
  }
  ans
}

#' Iteratively add items to a scale and recompute scale measures
#'
#' This allows you to take an item.scale and iteratively add new items.  It recalculates the scale and
#' also calculates new Cronbach's alpha for each new scale.
#'
#' This is useful during exploratory analysis in determining which items are needed in a scale.
#' Each item in \code{scope} will be added separately in the order they are given and a
#' new scale/alpha will be computed.
#'
#' @param x An \code{item.scale}
#' @param scope A character vector containing the list of items that will be added.  Items
#'   must be present either in the original data from of {x} or in the environment.
#' @return An \code{n} by 3 matrix where \code{n} is the number of items to be added.
#'   Includes the mean, standard deviation, and alpha of each new scale as items are added.
#' @export
add1.item.scale <- function(x, scope) {
  if (missing(scope)) {
    stop("Must specify scope variables to be added")
  }
  if (!is.character(scope)) {
    stop("Scope must be a character vector")
  }
  scale <- attributes(x)
  scale$data <- x
  ni <- length(scope)
  ans <- matrix(nrow=ni+1, ncol=3, dimnames=list(c("<none>",scope), c("Mean", "SD", "Alpha")))
  ans[1,1] <- mean(x, na.rm=T)
  ans[1,2] <- sd(x,na.rm=T)
  ans[1,3] <- attr(x, "cronbach")
  for (i in seq(ni)) {
    term <- scope[i]
    newfit <- update(scale, as.formula(paste("~ . + ", term)))
    ans[i+1,1] <- mean(newfit, na.rm=T)
    ans[i+1,2] <- sd(newfit,na.rm=T)
    ans[i+1,3] <- attr(newfit, "cronbach")
  }
  ans
}

#' Prints information about an \code{item.scale} object
#'
#' Outputs standard information found in an \code{item.scale}, such as number of items, number of
#' responses, scale mean, and Cronbach's alpha
#'
#' @param x An \code{item.scale} object
#' @param digits The number of digits to print when printing numbers.  Passed to \code{format()}
#' @export
print.item.scale <- function(x, digits=3) {
  if (attr(x, "standardized"))
    cat("Standardized ")
  cat("Scale of Multiple Survey Items\n")
  cat(paste("Items: \n  ", gsub(" \\+ ", ", ", as.character(attr(x, "formula"))[2]), "\n", sep=""))
  cat(paste("Mean: \n  ", format(mean(x, na.rm=T), digits=digits), " (", format(sd(x, na.rm=T), digits=digits), ")\n", sep=""))
  N <- sum(!is.na(x))
  missing <- sum(is.na(x))
  cat(paste("Responses: \n  ", format(N, digits=digits), "  (", format(missing, digits=digits), " missing)\n", sep=""))
  cat(paste("Cronbach's Alpha: \n  ", format(attr(x, "cronbach"), digits=digits), "\n", sep=""))
  #  cat(paste("Alternative Cronbach's Alpha: \n  ", format(attr(x, "altalpha"), digits=digits), "\n", sep=""))
}

#' Calculates additional summary data about an \code{item.scale}
#'
#' Calculates summary data such as individual item means and standard deviations
#'
#' @param x An \code{item.scale} object
#' @param maxsum Not sure what this is
#' @param digits Unused
#' @return An object of class \code{summary.item.scale}
#' @export
summary.item.scale <- function(x, maxsum=NULL, digits=NULL) {
  out <- c("Mean"=mean(x, na.rm=T), "SD" = sd(x, na.rm=T), "Cronbach"=attr(x, "cronbach"))
  if ((!is.null(maxsum)) && (length(out) > maxsum)) {
    out <- out[1:maxsum]
  }
  out
  attr(out, "data") <- x
  attr(out,"item.means") <- apply(attr(x, "model.frame"), 2, mean, na.rm=T)
  attr(out, "item.sds") <- apply(attr(x, "model.frame"), 2, sd, na.rm=T)
  class(out) <- c("summary.item.scale", "item.scale", "numeric")
  out
}

#' Prints out long summary information about an \code{item.scale}
#'
#' Prints out basic summary information, such as mean, sd, and cronbach's alpha.  Also prints out long
#' summary information, such as the mean and sd of each item, and the item-item correlations
#'
#' @param x An \code{item.scale} object
#' @param digits The number of digits to print when printing numbers.  Passed to \code{format()}
#' @export
print.summary.item.scale <- function(x, digits=3) {
  print.item.scale(attr(x, "data"), digits=digits)

  cat("\nItem Means:\n")
  temp <- data.frame(row.names=colnames(attr(attr(x, "data"), "model.frame")), Mean=attr(x,"item.means"), SD=attr(x, "item.sds"))
  print(temp, digits=digits)
  cat("\nItem-Item Correlations:\n")
  print(attr(attr(x, "data"), "item.cor"), digits=digits)
}

if (!exists("factanal.default")) {
  factanal.default <- stats::factanal
}

#' Conduct a factor analysis
#' @export
factanal <- function(x, factors, ...) {
  UseMethod("factanal")
}

#' Conduct a factor analysis on the items of an \code{item.scale}
#'
#' Extract the list of items in an \code{item.scale} and then conduct a maximum likelihood
#' factor analysis on that complete list of items
#'
#' @param x An \code{item.scale} that contains the list of items to factor analyze
#' @param factors the number of factors to extract
#' @return An object of class \code{factanal}
#' @export
factanal.item.scale <- function(x, factors, ...) {
  factanal.default(na.omit(attr(x, "model.frame")), factors, ...)
}

#library(psy) # cronbach
#library(gdata) # trim
#library(ggplot2)
#source("multiplot.R")


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
standardize <- function(x) {
  mean = mean(x, na.rm=T)
  sd = sd(x, na.rm=T)
  (x - mean) / sd
}

# Alternative way of calculating cronbach's alpha
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
#' @return An object of type \code{item.scale}.  This is fundamentally a vector, and can
#'   be used as such.  It has a number of additional attributes (such as \code{cronbach})
#'   that are useful in future calculations and functions
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

print.summary.item.scale <- function(x, digits=3) {
  print.item.scale(attr(x, "data"), digits=digits)

  cat("\nItem Means:\n")
  temp <- data.frame(row.names=colnames(attr(attr(x, "data"), "model.frame")), Mean=attr(x,"item.means"), SD=attr(x, "item.sds"))
  print(temp, digits=digits)
  cat("\nItem-Item Correlations:\n")
  print(attr(attr(x, "data"), "item.cor"), digits=digits)
}

# Note: You might need to change this hard-coded y limit
plot_question <- function(d, question, labels=NULL, ymax=1200) {
  d <- subset(d, !is.na(d[[question]]))
  out <- ggplot(d, aes_string(x=question)) + geom_histogram(binwidth=1) +
            theme_bw() + scale_x_discrete(drop=F) + ylim(0,ymax)
  if (!is.null(labels)) {
    out <- out + scale_x_discrete(labels=labels)
  }
  out
}

plot_list <- function(s, ...) {
  mf <- attr(s, "model.frame.orig")
  plots <- lapply(colnames(mf), function(n) { plot_question(mf, n, ...)})
  plots
}

plot.item.scale <- function(s, cols=NULL) {
  plots <- plot_list(s)
  if (is.null(cols)) {
    mf <- attr(s, "model.frame")
    cols <- floor(sqrt(dim(mf)[2]))
  }
  multiplot(plotlist=plots, cols=cols)
}


if (!exists("factanal.default")) {
  factanal.default <- factanal
}
factanal <- function(x, factors, ...) {
  UseMethod("factanal")
}
factanal.item.scale <- function(x, factors, ...) {
  factanal.default(na.omit(attr(x, "model.frame")), factors, ...)
}


# ***************************************
# ** Functions to make cleaning surveys easier
# ***************************************


# Convert a set of columns to a given type (given by fun)
apply_columns <- function(df, fun, cols, ...) {
  for (col in cols) {
    df[[col]] <- fun(df[[col]], ...)
  }
  df
}

# Take a set of columns and create _n version of them that are numeric (for Likerts)
make_columns_numeric <- function(df, cols, fun=as.numeric, ...) {
  for (col in cols) {
    df[[paste(col, "_n", sep="")]] <- fun(df[[col]], ...)
  }
  df
}

# Create strings from all combinations of its inputs
combine_names <- function(..., sep=".") {
  nargs <- length(args <- list(...))
  df <- expand.grid(args)
  chs <- apply(df, 2, as.character)
  if (length(df[,1]) > 1) {
    chs <- apply(chs, 2, trim)
  } else {
    chs <- trim(chs)
  }
  if (class(chs) == "character") {
    paste(chs, collapse=sep)
  } else {
    apply(chs, 1, paste, collapse=sep)
  }
}

# Make a formula from a combine_names series of variables
mf <- function(..., type="+") {
  temp <- combine_names(...)
  t2 <- paste("~ ", paste(temp, collapse=type))
  formula(t2)
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


# Function that processes a survey question: converts to the appropriate type and renames it
question <- function(survey, col, new_name=col, question_type=checkbox, ...) {
  survey[[col]] <- question_type(survey[[col]], ...)
  names(survey)[names(survey) == col] <- new_name # Rename column
  survey
}

# Generic function to handle multiple choice functions
multiple_choice <- function(x, levs, labs=levs, ordered=TRUE, reverse=FALSE) {
  if (reverse) {
    levs <- rev(levs)
  }
  factor(x, levels=levs, labels=labs, ordered=ordered)
}

# function to change NAs to 0's
na_zero <- function(x) { x[is.na(x)] <- 0; x}

# functions to help with the different types of Questions
emotions <- function(x, levs=emotion_levels, labs=levs, ordered=F, reverse=F) { multiple_choice(x, levs=levs, labs=labs, ordered=ordered, reverse=reverse) }
familiarity <- function(x, levs=familiarity_levels, ...) { multiple_choice(x, levs=levs, ...)}
comfortable <- function(x, levs=comfort_levels, ...) { multiple_choice(x, levs=levs, ...)}
concern <- function(x, levs=concern_levels, ...) { multiple_choice(x, levs=levs, ...)}
agree <- function(x, levs=agree_levels, ...) { multiple_choice(x, levs=levs, ...)}
checkbox <- function(x, lev=sort(levels(x)), labs=c("No", "Yes")) { ordered(x, levels=lev, labels=labs)}
frequency <- function(x, levs=frequency_levels, ...) { multiple_choice(x, levs=levs, ...)}
yesno <- function(x, levs=yesno_levels, ordered=F, ...) { multiple_choice(x, levs=levs, ordered=ordered, ...)}
likely <- function(x, levs=likely_levels, ...) { multiple_choice(x, levs=levs, ...)}
understanding <- function(x) { ordered(x, levels=c("None", "Little", "Some", "Good", "Full"))}
education <- function(x, levs=education_levels, ...) { multiple_choice(x, levs=levs, ...)}
gender <- function(x, levs=gender_levels, ordered=F, ...) { multiple_choice(x, levs=levs, ordered=ordered, ...)}
race <- function(x, levs=race_levels, ordered=F, ...) { multiple_choice(x, levs=levs, ordered=ordered, ...)}
attention <- function(x, correct) { x == correct }


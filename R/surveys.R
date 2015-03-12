
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

# TODO: Write a column type auto-detectors


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


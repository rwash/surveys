######################
# Helper functions that make it easier to analyze chooseone and chooseany questions
#   (aka a set of TRUE/FALSE columns that logically represent a single set of options)

default_width=200

#' Select names from a tibble, and returns a character vector
#'
#' @param data The dataset (data frame or tibble)
#' @param ... tidy-select name selection
#'
#' @return Character vector of column names
#' @export
#'
#' @examples
ns <- function(data, ...) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data)
  names(pos)
}

combo_condition <- function(...) {
  columns <- rlang::list2(...) # Automatically quote column names
  condition <- columns %>% purrr::map(sym) %>%   # Convert column names to symbols
    purrr::map(~expr(!!.x == TRUE)) %>%          # Map to R expression <name> == TRUE (possibly unnecessary)
    purrr::reduce(~expr((!!.x) & (!!.y)))        # Reduce to combine into a single expression
  condition
}

# Function that counts the number of rows with both values true
count_entries <- function(data, ...) {
  # Create the logical condition (A & B) from the combination of names in the ... parameters.  Names are automatically quoted
  condition <- combo_condition(...)
  # Calculate the count by applying that condition and counting the number TRUE
  data %>% summarize(sum(!!condition) ) %>% as.numeric()
}

# Each variable supports tidy-select syntax, so you can use :, starts_with, etc. to specify the set of columns that you want to be a variable
# Each entry in ... is a separate tidy-select.  If you want to use multiple conditions, combine them with c()
crosstab_chooseany <- function(data, ..., .y=NULL) {
  pseudo_vars <- rlang::enquos(...) # Quasi-quote parameters; Enable tidy-select
  all_vars <- pseudo_vars %>% purrr::map(~ns(data, !!.x)) # For each parameter, grab the list of names
  # Assign names, either from the original parameter names, or automatically (..1, etc.)
  all_vars <- set_names(all_vars, vctrs::vec_as_names(names(all_vars), repair="universal") )
  # Create a grid of all combinations
  grid <- do.call(expand_grid, all_vars)
  # Fill in counts
  grid$count <- purrr::pmap_dbl(grid, ~count_entries(data, ...))
  grid
}

# Function that takes a single logical chooseany variable (set of columns) and produces an internal crosstab:
#  How many of each combination of values there is.
#  ... Is using tidy-select syntax
crosstab_internal <- function(data, ...) {
  pseudo_vars <- ns(data, ...)
  out <- expand_grid(one=pseudo_vars, two=pseudo_vars)
  out$count <- purrr::pmap_dbl(out, ~count_entries(data, ...))
  out
}

widen <- purrr::partial(tidyr::pivot_wider, names_from="two", values_from="count")
widen_perc <- function(data, ...) { data %>% add_perc(count, ...) %>% select(-count) %>% tidyr::pivot_wider(names_from="two", values_from="perc")}
widen_perc_col <- function(data, ...) { data %>% filter(one != two) %>% add_perc(count, N, margins=one:two) %>% select(-count, -perc, -perc_one) %>% pivot_wider(names_from=two, values_from=perc_two)}

#
#' Add percentage column
#'
#' Adds a percentage column based on the counts in the count_col.  Automatically quoted.
#' .total is to specify a total count (e.g. in chooseany columns, where the sum is not the same as the total N)
#' name is the name of the column to add
#' margins is which columns to add margins for (tidy-select)
#'
#' @param x The dataset
#' @param count_col The name of the column (tidy-select) that contains which column should be counted
#' @param .total (optional) The name of the column containing totals
#' @param name The name of the new column being added with the percentage (default: "perc")
#' @param margins
#' @param format
#'
#' @return
#' @export
#'
#' @examples
add_perc <- function(x, count_col = NULL, .total=NULL, name="perc", margins=NULL, format=TRUE) {
  if (is.null(.total)) {
    temp <- x %>% add_tally({{count_col}}, name=".tally")
  } else {
    temp <- mutate(x, .tally = .total)
  }
  out <- temp %>% mutate(!!name :=  {{count_col}} / .tally) %>% select(-`.tally`)
  if (format) {
    out <- out %>% mutate(!!name := percent(.data[[name]]))
  }
  margin_col_names <- ns(x, c({{margins}}, -{{count_col}}))
  if (length(margin_col_names) > 0) {
    for (col in margin_col_names) {
      out <- out %>% group_by(.data[[col]]) %>% add_perc({{count_col}}, name=str_c(name, col, sep="_"), format=format) %>% ungroup()
    }
  }
  out
}

grouped_crosstab <- function(data, group,  ...) {
  crosstab_chooseany(data, ...)
}

# Grab stories matching criteria.   ... is passed into filter() for the dataset, which effectively ANDs things together
stories <- function(.data, ..., limit=NULL) {
  temp <- data %>% filter(...) %>% select(id, story=full_story)
  if (!is_null(limit)) {
    temp <- temp %>% head(limit)
  }
  set_names(temp$story, temp$id)
}


#' Summarize a chooseany question
#'
#' @param data Data frame or tibble with the dataset
#' @param question Which survey question, as a string.
#' @param width Truncates question names that are too long to width
#'
#' Since this attempts to summarize a choose-any-of-the-options style question
#' (so multiple answers are possible), the function assumes that each answer is
#' a separate column, and that all columns start with the same string (the name
#' of the question), are followed by an underscore, followed by the value of the
#' option.   The function uses starts_with() to identify the appropraite set of
#' columns to summarize (which is why question should be a string)
#'
#' @return
#' @export
#'
#' @examples
summarize_chooseany <- function(data, question, width=default_width, ...) {
  val_name <- str_c(question, "_value")
  prefix <- str_c(question, "_")
  data %>%
    select(starts_with(question), -any_of(val_name), -ends_with("_TEXT")) %>%
    crosstab_chooseany("{question}" := starts_with(question)) %>%
    add_perc(count, N) %>%
    mutate("{question}" := str_remove(.data[[question]], prefix)) %>%
    mutate("{question}" := str_trunc(.data[[question]], width))
}


#' Summarizes a multiple choice style question
#'
#' @param data Data frame or tibble with the dataset
#' @param question Which survey question, as a string.
#' @param width Truncates question names that are too long to width
#'
#' @return
#' @export
#'
#' @examples
summarize_chooseone <- function(data, question, width=default_width, ...) {
  data %>%
    group_by(.data[[question]]) %>%
    summarize(count=n()) %>%
    add_perc(count) %>%
    mutate("{question}" := str_trunc(as.character(.data[[question]]), width))
}

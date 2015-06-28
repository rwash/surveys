
char_is_numeric <- function(x) { return(all(grepl("^[-+]?[1-90]+(\\.[1-90]*)?$", x) | is.na(x))) }
char_is_logical <- function(x) { return(all(grepl("^(false)$|^(true)$|^f$|^t$|^0$|^1$", x, ignore.case=T) | is.na(x)))}
char_is_checkbox <- function(x) { return(F)} # How is checkbox different from logical?s
char_is_ip <- function(x) { return(all(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$", x) | is.na(x))) }
is_qualtrics_subject_id <- function(x) { return(all(grepl("^R_[a-zA-Z0-9]{15}$", x) | is.na(x)))}
all_identical <- function(x) { return(length(unique(x)) == 1)}
char_is_date <- function(x) { return(all(!is.na(suppressWarnings(lubridate::ymd_hms(x))))) }



convert_logical <- function(x) { return(sub("0", "F", sub("1", "T", x))) }

checkbox <- function(x) { sapply(x, isTRUE) } # How is checkbox different from logical?



load_question_types <- function() {
  add_question_type("numeric", char_is_numeric, as.numeric)
  add_question_type("logical", char_is_logical, function(x) { as.logical(convert_logical(x)) } )
  add_question_type("ip.address", char_is_ip, as.character)
  add_question_type("qualtrics.subject_id", is_qualtrics_subject_id, as.character)
#  add_question_type("no_variation", all_identical, "remove")
  add_question_type("date", char_is_date, lubridate::ymd_hms)
#  add_question_type("checkbox")
}

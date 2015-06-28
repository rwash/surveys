
char_is_numeric <- function(x) { return(all(grepl("^[-+]?[1-90]+(\\.[1-90]*)?$", x) | is.na(x))) }
char_is_logical <- function(x) { return(all(grepl("^(false)$|^(true)$|^f$|^t$|^0$|^1$", x, ignore.case=T) | is.na(x)))}
char_is_checkbox <- function(x) { return(F)} # How is checkbox different from logical?s
char_is_ip <- function(x) { return(all(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$", x) | is.na(x))) }

convert_logical <- function(x) { return(sub("0", "F", sub("1", "T", x))) }

checkbox <- function(x) { sapply(x, isTRUE) }
#surveys::add_question_type("test_checkbox", is.logical, checkbox)



load_question_types <- function() {
  add_question_type("numeric", char_is_numeric, as.numeric)
  add_question_type("logical", char_is_logical, function(x) { as.logical(convert_logical(x)) } )
  add_question_type("ip.address", char_is_ip, as.character)
#  add_question_type("checkbox")
}

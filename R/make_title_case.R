##
make_title_case <- function(x) {
  #' Convert a string into Title Case.
  #'
  #' @description If a string arrives as "hello uSEr" or "HELLO user", etc, this
  #'   function changes it to "Hello User". Here, "Title Case" means the first
  #'   letter of each word is capitalised. Words are defined by the space
  #'   between them.
  #'
  #' @param x "string": The string to turn into title case. Can be a vector.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # From [https://stackoverflow.com/questions/43592419/convert-to-sentence-case-in-r]().
  # y <- gsub("([[:alpha:]])([[:alpha:]]+)", "\\U\\1\\L\\2", x, perl = TRUE)

  # From tolower documentation; can't handle vectors?
  # y <- tolower(x)
  # y <- strsplit(y, " ")[[1]]
  # y <- paste(toupper(substring(y, 1, 1)),
  #            substring(y, 2),
  #            sep = "", collapse = " ")

  # Piping to handle vectors
  y <- paste(toupper(x) |>
             strsplit(" ") |>
             lapply("[[", 1) |>
             substring(1, 1),   # now we have the first letter capitalised
           tolower(x) |>
             substring(2),      # lower case for everything but the first
           sep = "")            # join them back together!

  # Handle if it was a list
  if ("list" %in% is(x)) {
    y <- setNames(as.list(y), names(x))
  }

  return(y)
}

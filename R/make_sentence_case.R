##
make_sentence_case <- function(x) {
  #' Convert a string into sentence case.
  #'
  #' @description If a string arrives as "hello" or "HELLO" (or e.g. "heLLo"),
  #'   this function changes it to "Hello". I took the code directly from:
  #'   [https://stackoverflow.com/questions/43592419/convert-to-sentence-case-in-r]().
  #'   I do not really understand how it works.
  #'
  #' @param x "string": The string to turn into sentence case.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  y <- gsub("([[:alpha:]])([[:alpha:]]+)", "\\U\\1\\L\\2", x, perl = TRUE)

  return(y)
}

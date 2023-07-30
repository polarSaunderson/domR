count_decimal_places <- function(x) {
  #' Counts how many digits are after the decimal point
  #'
  #' @description This function is useful when we are automating things and need
  #'   to round, but aren't always sure what precision we need to round to. The
  #'   code was taken directly from this stackoverflow answer by daroczig:
  #'   [https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r](),
  #'   and I do not really understand how it works.
  #'
  #' @param x numeric: The value
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)),
                   ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

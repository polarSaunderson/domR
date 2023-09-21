count_decimal_places <- function(x) {
  #' Counts how many digits are after the decimal point
  #'
  #' @description This function is useful when we are automating things and need
  #'   to round, but aren't always sure what precision we need to round to. The
  #'   code was taken from this stackoverflow answer by daroczig:
  #'   [https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r](),
  #'   and I do not entirely understand how it works.
  #'
  #' @param x numeric: The value
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Preallocate
  xDP <- integer(length(x))
  machPrec <- .Machine$double.eps^0.5 # computer precision is min. diff possible

  # Loop through values
  for (ii in x) {
    iiIndex <- which(x == ii)
    if (!is.na(ii)) {
      if (abs(ii - round(ii)) > machPrec) { # diff is enough for computer to see
        # convert to text to count
        iiSplit <-  strsplit(sub('0+$', '',
                                 format(ii, digits = 15,
                                        scientific = FALSE)), # not Xe+Y
                             ".", fixed = TRUE)[[1]]
        if (length(iiSplit) == 1) {       # if nothing after decimal
          xDP[iiIndex] <- 0
        } else {                          # get part after decimal place
          xDP[iiIndex] <- nchar(iiSplit[[2]])
        }
      } else {
        xDP[iiIndex] <- 0
      }
    } else {
      xDP[iiIndex] <- NaN
    }
  }
  return(xDP)
}

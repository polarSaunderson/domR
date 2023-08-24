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
  #
  # if (abs(x - round(x)) > .Machine$double.eps^0.5) {
  #   nchar(strsplit(sub('0+$', '', as.character(x)),
  #                  ".", fixed = TRUE)[[1]][[2]])
  # } else {
  #   return(0)
  # }
# }

calc_magnitude <- function(x) {
  #' Return the magnitude of a number
  #'
  #' @description Magnitudes are considered powers of 10, so 1:9.999... are all
  #'   magnitude 0, 10-99.999... are magnitude 1, 0.1:0.999... are -1, etc. The
  #'   sign of the number makes no difference.
  #'
  #' @param x The number.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  showCat <- FALSE # only here for testing purposes

  xMag <- integer(length(x))
  for (ii in x) {
    iiIndex <- which(x == ii)
    iiString <- format(abs(ii), scientific = FALSE)
      cat2(iiString, showCat)
    iiSplit  <- strsplit(iiString, split = ".", fixed = TRUE)
      cat2(iiSplit, showCat)

    # Approach depends on number magnitude
    if (abs(ii) < 1) {
      iiDecimals  <- iiSplit[[1]][[2]]
        cat2(iiDecimals, showCat)
      iiDeciSplit <- strsplit(iiDecimals, "") |> unlist() |> as.numeric()
        cat2(iiDeciSplit, showCat)
      iiNonZeroes <- which(iiDeciSplit != 0)
        cat2(iiNonZeroes, showCat)
      firstNonZero <- iiNonZeroes[1]
        cat2(firstNonZero, showCat)
      xMag[iiIndex] <- -firstNonZero
    } else {
      iiIntegers <- iiSplit[[1]][[1]]
        cat2(iiIntegers, showCat)
      iiIntSplit <- strsplit(iiIntegers, "")
        cat2(iiIntSplit, showCat)
      iiLength <- length(iiIntSplit[[1]])
        cat2(iiLength, showCat)
      xMag[iiIndex] <- iiLength - 1
    }
  }
  return(xMag)
}

#' calc_magnitude <- function(x) {
#'   #' Return the magnitude of a number
#'   #'
#'   #' @description Simply counts how many numbers are to the left of the decimal
#'   #'   point. Returns zero if the absolute value is less than 1.
#'   #'
#'   #' @param x The number
#'   #'
#'   #' @export
#'
#'   # Code -----------------------------------------------------------------------
#'   xMag <- integer(length(x))
#'   for (ii in x) {
#'     iiIndex <- which(x == ii)
#'     if (abs(ii) < 1) {
#'       xMag[iiIndex] <- 0
#'     } else {
#'       xMag[iiIndex] <- strsplit(as.character(round(ii)), "") |>
#'         unlist() |> length()
#'     }
#'   }
#'   return(xMag)
#' }

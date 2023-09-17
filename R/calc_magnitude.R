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

  # preallocate
  xMag <- integer(length(x))

  # calc magnitude for all values in the vector
  for (ii in x) {
    iiIndex <- which(x == ii)
    if (ii == 0) {
      xMag[iiIndex] <- 0    # guard
    } else {
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
  }
  return(xMag)
}

calc_mode <- function(x) {
  #' Return the mode average - most common value/s
  #'
  #' @description R has built in mean and median functions, but not mode.
  #'
  #' @param x The data to find the mode of.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  allX   <- unique(x)
  countX <- match(x, u) |> tabulate()
  if (length(countX) != length(x)) {
    allX[countX == max(countX)]
  } else {
    warning("No mode: all are equal!")
  }
}

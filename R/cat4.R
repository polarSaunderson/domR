cat4 <- function(x, show = TRUE, skipCatCall = TRUE) {
  #' Print out a variable's name, value and the location
  #'
  #' @description This function combines [which_line()] and [cat2()]. It
  #'   therefore shows the value and name of a variable, and the location of it.
  #'   This is useful for helping to debug when writing functions.
  #'
  #' @param x The variable to show.
  #' @inheritParams cat2
  #' @inheritParams which_line
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (isTRUE(show)) {
    print_line("-", 1, 0)
    xName <- deparse(substitute(x))
    if (is.list(x)) {
      cat_list(x, show = show, name = xName)
    } else {
      cat2(x, show = show, name = xName)
    }
    which_line(skipCatCall = TRUE)
  }
  return(invisible(x))
}

cat3 <- function(..., nStart = 1, nEnd = 1, show = TRUE) {
  #' Print out input easier
  #'
  #' @description This function is simply [cat()] but optionally adds newlines
  #'   (`\n`) before and/or after to quicken things up.
  #'
  #'   **Note:** The output format is not always exactly the same as [cat()]. It
  #'   is still in progress.
  #'
  #' @param ... The strings to display. The values are collapsed down with a
  #'   space between them and then shown using [cat()]. Not well-tested with
  #'   complex data types - use with caution! If you want the name of the
  #'   variable and the variable, use [cat2()]; if the location too, use
  #'   [cat4()].
  #' @param show logical If FALSE, the function does nothing; it is useful if
  #'   there are multiple [cat3()] calls throughout; set a variable at the
  #'   beginning and use that as the 'show' argument in each [cat3()] call.
  #'   Simply changing that to FALSE allows the function to be run without all
  #'   the calls, but the calls can remain in place if further [cat3()] use is
  #'   likely.
  #' @inheritParams print_line
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (isTRUE(show)) {
    dots <- list(...)
    dots <- paste(dots, collapse = " ")
    cat(rep("\n", nStart), dots, rep("\n", nEnd))
  }
}

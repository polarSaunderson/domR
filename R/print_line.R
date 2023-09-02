##
print_line <- function(lineType = "=", nStart = 1, nEnd = 1, width = 22){
  #' Print a line to the console to help delineate output
  #'
  #' @description Useful when working interactively in RStudio, and can be
  #'   particularly helpful to group output to the console or when testing.
  #'
  #' @param lineType "string": The symbol/s that will be printed to create a
  #'   line across the console.
  #' @param nStart numeric: How many `\n` should be included *before* the line?
  #' @param nEnd numeric: How many `\n` should be included *after* the line?
  #' @param width numeric: How many times should the above be repeated? Usually
  #'   unnecessary, but change it if screen is particularly wide or narrow.
  #'
  #' @examples
  #'   print_line()
  #'   print_line("=", 1, 0)
  #'   print_line("-", width = 10)
  #'   print_line("+-", width = 25)
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  lineString <- paste0(rep(lineType, width * 3), collapse = "")
  cat(rep("\n", nStart), lineString, rep("\n", nEnd), sep = "")
}

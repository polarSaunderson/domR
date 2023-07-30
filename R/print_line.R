##
print_line <- function(lineType = "=", width = 22){
  #' Print a line to the console to help delineate output
  #'
  #' @description Useful when working interactively in RStudio, and can be
  #'   particularly helpful to group output to the console or when testing.
  #'
  #' @param lineType "string": The symbol/s that will be printed to create a
  #'   line across the console.
  #' @param width numeric : How many times should the above be repeated? Usually
  #'   unnecessary, but change it if screen is particularly wide or narrow.
  #'
  #' @examples
  #'   print_line()
  #'   print_line("-", 10)
  #'   print_line("+-", 25)
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  lineString <- paste0(rep(lineType, width * 3), collapse = "")
  cat("\n", lineString, "\n", sep = "")
}

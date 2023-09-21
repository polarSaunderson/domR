##
print_line <- function(lineType = "=",
                       nStart = 1,
                       nEnd = 1,
                       width = 22,
                       show = TRUE){
  #' Print a line to the console to help delineate output
  #'
  #' @description Useful when working interactively in RStudio, and can be
  #'   particularly helpful to group output to the console or when testing.
  #'
  #' @param lineType "string": The symbol/s that will be printed to create a
  #'   line across the console. A numeric value between 1 and 9 can be used,
  #'   with the following defined:
  #'
  #'   Numeric Options
  #'   ```
  #'       1      ---------------- 1
  #'       2      ================ 2
  #'       3      ________________ 3
  #'       4      ~~~~~~~~~~~~~~~~ 4
  #'       5      ++++++++++++++++ 5
  #'       6      >>>>>>>>>>>>>>>> 6
  #'       7      <<<<<<<<<<<<<<<< 7
  #'       8      |||||||||||||||| 8
  #'       9      !!!!!!!!!!!!!!!! 9
  #'   ```
  #'   All other numbers will default to "-" (equivalent to 1). To use a line of
  #'   the number itself, enclose it as a string: "1". See example.
  #' @param nStart numeric: How many `\n` should be included *before* the line?
  #' @param nEnd numeric: How many `\n` should be included *after* the line?
  #' @param width numeric: How many times should the 'lineType' be repeated?
  #'   Usually unnecessary, but change it if screen is particularly wide or
  #'   narrow.
  #' @param show LOGICAL; should the line be printed? Can be set as FALSE along
  #'   with [cat2()] so it can be easily switched on or off for debugging.
  #'
  #' @examples
  #'   print_line()
  #'   print_line(1)
  #'   print_line("1")
  #'   print_line(2)
  #'   print_line("=", 1, 0)
  #'   print_line("-", width = 25)
  #'   print_line("?", width = 10)
  #'   print_line("+-", width = 10)
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  if (isTRUE(show)) {
    if (is.numeric(lineType)) {
      lineType <- switch(lineType,
                         "-", "=", "_", "~", "+", ">", "<", "|", "!", "\u00B0")
    }
    if (is.null(lineType)) lineType <- "-"

    # Create string
    lineString <- paste0(rep(lineType, width * 3), collapse = "")

    # Display
    cat(rep("\n", nStart), lineString, rep("\n", nEnd), sep = "")
  }
}

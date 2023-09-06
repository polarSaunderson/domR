print_on_saving <- function(filename = "") {
  #' Let the user know the save didn't throw an error & print relevant info
  #'
  #' @description Just used interactively to verify that something has been
  #'   saved. We also show the filename and path, and the last git commit's SHA.
  #'   Make sure to place this function after the save call, because it doesn't
  #'   actually check anything at all - it just assumes that if it has been
  #'   called, the save went ahead okay.
  #'
  #' @param fileName "string": Where should it have been saved?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  print_line()
  cat(get_file_codes(),"\n",
      "   Successfully saved to file! \n",
      "->", filename)
  print_line()
}

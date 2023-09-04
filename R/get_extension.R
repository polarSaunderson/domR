get_extension <- function(filename) {
  #' Extract the file extension from a filename
  #'
  #' @description A very simple function that takes whatever is to the right of
  #'   the last "." in a filename and considers that as the file extension.
  #'
  #' @param filename The filename we want the extension from.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Figure out extension
  ext <- strsplit(filename, split = ".", fixed = TRUE)[[1]] |>
    (function(x) x[length(x)]) ()
  return(ext)
}

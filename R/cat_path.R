cat_path <- function(...) {
  #' Concatenate input into a filepath
  #'
  #' @description A very simple function that simply collapses the input down,
  #'   separating each part with a "/" to create a filepath. It is very similar
  #'   to file.path, but just my (slower) implementation.
  #'
  #' @param ... The parts of the file path. Each part will be separated with a
  #'   "/". Can handle vectors or lists.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xx <- list(...)
  yy <- unlist(xx) |> paste(collapse = "/")
  zz <- paste0(yy, "/")
  return(zz)
}

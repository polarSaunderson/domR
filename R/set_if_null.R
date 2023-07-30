set_if_null <- function(x, defaultValue) {
  #' Sets a default value for a NULL argument
  #'
  #' @description In some functions, it is necessary to create an argument that
  #'   has a default value defined by the other arguments (i.e. calculated
  #'   within the function), but that can nevertheless be overwritten if
  #'   required. We therefore set the argument value as NULL, and can use this
  #'   function within the function to determine whether the argument's value
  #'   needs to be calculated or not. This function just makes it cleaner,
  #'   clearer and quicker to see what is happening compared to the default
  #'   syntax for this process.
  #'
  #' @param x Which argument is being checked?
  #' @param defaultValue What's the value if the argument is NULL?
  #'
  #' @examples
  #'   customisableArgument <- NULL
  #'   customisableArgument <- set_if_null(customisableArgument, 0.5)
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (is.null(x)) x <- defaultValue
  return(x)
}

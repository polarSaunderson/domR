cat5 <- function(x, show = TRUE, name = NULL) {
  #' Print out a variable's name, class and value
  #'
  #' @description Using [cat5()] prints out a variable, it's class and it's name.
  #'
  #'   This function is useful because I don't know how to debug "correctly".
  #'   For example, when developing functions, it can be hard to know where the
  #'   code is going wrong, particularly if there are no true errors.  However,
  #'   the returned value can still be incorrect, but it is not clear at which
  #'   stage a variable has which value.
  #'
  #'   Sprinkling the [cat5()] function at key locations in functions allows us
  #'   to see what is happening where within a function.
  #'
  #' @param x The variable that should be displayed. It's name and value will be
  #'   shown. Lists are fed to [cat_list()].
  #'
  #'   If you just want the variable (and not the name and class), stick with
  #'   [cat()], or use [cat3()], which basically adds a newline `\n` before and
  #'   after x; if just the name and value, use [cat2()].
  #'
  #'   **Note:** This function has not been tested with complex data types - use
  #'   with caution!
  #'
  #' @param show LOGICAL: If FALSE, the function just returns the input but does
  #'   not print anything out. This behaviour is helpful as it allows multiple
  #'   [cat5()] calls to be added throughout a function, and a single variable
  #'   at the start can be used to turn the printing on for debugging, or off
  #'   for use. Useful when it is likely that further debugging will be needed.
  #' @param name "string": Should an alternative name be used? By default, the
  #'   name of 'x' is used. However, if [cat5()] is nested, this will likely
  #'   just end up as "x" for every variable, so the true name can be entered
  #'   here.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (isTRUE(show)) {
    # Prepare name
    if (is.null(name)) {
      name <- deparse(substitute(x))
    }

    # Prep data
    if (is.list(x)) {
      cat_list(x, name = name)
    } else {
      if (length(x) > 1) {
        x[is.null(x)] <- "NULL"      # let us know if it is NULL
      }

      # Display
      cat(paste0("\n", name, " (", class(x), ") :"), x, "\n", fill = TRUE)
    }
  } else if (!isFALSE(show)){
    warning("Only one variable can be used at once in cat5!")
  }
  return(invisible(x))
}

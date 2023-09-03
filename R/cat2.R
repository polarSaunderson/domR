cat2 <- function(x, show = TRUE, name = NULL) {
  #' Concatenate the name and variable
  #'
  #' @description This function is likely useful because I don't know how to
  #'   debug "correctly". When developing functions, it can be hard to know
  #'   where the code is going wrong. Using `cat2()` prints out a variable and
  #'   it's name. Sprinkle these at key locations in functions to see what is
  #'   happening where. If it is a list, try [cat_list()].
  #'
  #' @param x The variable that should be displayed. It's name and value will be
  #'   shown. Lists are unlisted. Not tested with complex data types - use with
  #'   caution! If you just want the variable (and not the name), stick with
  #'   `cat()`, or use `cat3()`, which basically adds "\n" before and after x.
  #' @param show LOGICAL: If FALSE, the function does nothing; it is useful if
  #'   there are multiple `cat2()` calls throughout; set a variable at the
  #'   beginning and use that as the 'show' argument in each `cat2()` call.
  #'   Simply changing that to FALSE allows the function to be run without all
  #'   the calls, but the calls can remain in place if further `cat2()` use is
  #'   likely.
  #' @param name "string": Should an alternative name be used? By default,
  #'   the name of x is used. However, if [cat2()] is nested, this will likely
  #'   just end up as "x" for every variable.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (isTRUE(show)) {
    # Prepare name
    if (is.null(name)) {
      name <- deparse(substitute(x))
    }

    # Prep data
    x <- unlist(x)                 # cat doesn't work with lists
    if (length(x) > 1) {
      x[is.null(x)] <- "NULL"      # let us know if it is NULL
    }

    # Display
    cat(paste0("\n", name, " :"), x, "\n")
  }
}

cat3 <- function(..., show = TRUE) {
  #' Concatenate a variable
  #'
  #' @description This function is simply `cat()` but adds a newline "\n" before
  #'   and after to quicken things up.
  #'
  #' @param ... The variable/s that should be displayed. The values will be
  #'   collapsed down with a space between them and then shown using `cat()`.
  #'   Not tested with complex data types - use with caution! If you want the
  #'   name of the variable and the variable, use `cat2()`.
  #' @param show logical If FALSE, the function does nothing; it is useful if
  #'   there are multiple `cat3()` calls throughout; set a variable at the
  #'   beginning and use that as the 'show' argument in each `cat3()` call.
  #'   Simply changing that to FALSE allows the function to be run without all
  #'   the calls, but the calls can remain in place if further `cat3()` use is
  #'   likely.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (isTRUE(show)) {
    dots <- list(...)
    dots <- paste(dots, collapse = " ")
    cat("\n", dots, "\n")
  }
}


cat4 <- function(x, show = TRUE, skipCatCall = TRUE) {
  #' Concatenate the name and variable, and show the location
  #'
  #' @description This function combines [which_line()] and [cat2()]. It
  #'   therefore shows the value and name of a variable, and the location of it.
  #'
  #' @param x The variable to show.
  #' @inheritParams cat2
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (isTRUE(show)) {
    print_line("-", 1, 0)
    cat2(x, show = show, name = deparse(substitute(x)))
    which_line(skipCatCall = skipCatCall)
  }
}

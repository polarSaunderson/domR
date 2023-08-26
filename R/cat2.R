cat2 <- function(x, show = TRUE) {
  #' Concatenate the name and variable
  #'
  #' @description This function is likely useful because I don't know how to
  #'   debug "correctly". When developing functions, it can be hard to know
  #'   where the code is going wrong. Using `cat2()` prints out a variable and
  #'   it's name. Sprinkle these at key locations in functions to see what is
  #'   happening where.
  #'
  #' @param x The variable that should be displayed. It's name and value will be
  #'   shown. Lists are unlisted. Not tested with complex data types - use with
  #'   caution! If you just want the variable (and not the name), stick with
  #'   `cat()`, or use `cat3()`, which basically adds "\n" before and after x.
  #' @param show logical If FALSE, the function does nothing; it is useful if
  #'   there are multiple `cat2()` calls throughout; set a variable at the
  #'   beginning and use that as the 'show' argument in each `cat2()` call.
  #'   Simply changing that to FALSE allows the function to be run without all
  #'   the calls, but the calls can remain in place if further `cat2()` use is
  #'   likely.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if (isTRUE(show)) {
    naming <- deparse(substitute(x))
    x <- unlist(x)
    x[is.null(x)] <- "NULL"
    cat("\n", naming, ":", x, "\n")
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

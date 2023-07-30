`%notIn%` <- function(x, y) {
  #' The opposite of the native "in" function.
  #'
  #' @description If we want to know what isn't in something else, use this! It
  #'   returns a vector of binary values, not the actual values that are not in
  #'   there.
  #'
  #' @param x the values we are interested in
  #' @param y the values to compare against
  #'
  #' @examples
  #'   x <- c(1, 2, 3)
  #'   y <- c(2, 3, 4, 5)
  #'   z1 <- x %notIn% y    # should be TRUE,  FALSE, FALSE
  #'   z2 <- y %notIn% x    # should be FALSE, FALSE, TRUE, TRUE
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  match(x, y, nomatch = 0) == 0
  # x[match(x, y, nomatch = 0) == 0]   # alternative returns actual values
}




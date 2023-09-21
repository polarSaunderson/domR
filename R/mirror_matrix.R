mirror_matrix_horizontally <- function(x) {
  #' Mirrors a matrix so the left is right and the right is left
  #'
  #' @description Sometime the matrix is not in the orientation we want. Use
  #'   this function if you want to mirror it across a vertical line down the
  #'   centre of the matrix, so that the columns on the left move to the right,
  #'   and the columns on the right move to the left.
  #'
  #' @param x matrix: The matrix to mirror.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  y <- x[, ncol(x):1, drop = FALSE]
  return(y)
}

mirror_matrix_vertically <- function(x) {
  #' Mirrors a matrix so the top is bottom and the bottom is top
  #'
  #' @description Sometime the matrix is not in the orientation we want. Use
  #'   this function if you want to mirror it across a horizontal line through
  #'   the centre of the matrix, so that the rows on the top move to the bottom,
  #'   and the rows on the bottom move to the top
  #'
  #' @param x matrix: The matrix to mirror.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  y <- x[nrow(x):1, , drop = FALSE]
  return(y)
}

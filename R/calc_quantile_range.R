calc_quantile_range <- function(x, lower = 0.1, upper = 0.9, ...) {
  #' Find custom interquantile ranges
  #'
  #' @description Equivalent to the IQR, but not restricted to the 25th & 75th
  #'   percentiles. Simply calls [stats::quantile()], and takes the lower value
  #'   from the upper value.
  #'
  #' @param lower The lower probability.
  #' @param upper The upper probability.
  #' @param ... Any other arguments that can be accepted by [stats::quantile()],
  #'   except 'probs'.
  #' @inheritParams stats::quantile
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  y <- stats::quantile(x, probs = c(lower, upper), ...)
  z <- y[2] - y[1]
  names(z) <- paste0(lower*100, "-", upper*100, "%")
  return(z)
}

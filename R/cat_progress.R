cat_progress <- function(progress, total) {
  #' A simple progress bar
  #'
  #' @description This function displays the progress on the console. It is a
  #' simple function that is just here as a simple test; the intention is to
  #' develop it further.
  #'
  #' @param progress How many steps have been completed?
  #' @param total How many steps are there in total?
  #'
  #' @examples
  #' # Watch it in progress
  #' for (ii in 1:15) {
  #'   cat_progress(ii, 15)
  #'   Sys.sleep(0.2)
  #' }
  #'
  #' # Staggered output to observe in time
  #' for (ii in 1:8) {
  #'   cat_progress(ii, 15)
  #'   cat("\n")
  #' }
  #'
  #' # More "realistic" use
  #' # fake function
  #' tst_fun <- function(x) {
  #'   # pretend we're doing something more involve & slower!
  #'   yTime <- sample(c(0:100), 10, replace = TRUE) / 100
  #'   Sys.sleep(yTime)
  #'   y <- x^3
  #'   return(y)
  #' }
  #'
  #' # fake data
  #' xx <- sample(1:1000, 15)
  #' tt <- rep("", length(xx)) # preallocate
  #'
  #' # loop
  #' for (ii in seq_along(tt)) {
  #'   tst_fun(ii)
  #'   cat_progress(ii, length(tt))
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  percentage <- round(progress / total * 100) # numeric value
  scale      <- 24 / total                    # to scale values for 24 bit bars
  progBits   <- floor(progress * scale)       # progress bits
  emptyBits  <- 24 - progBits                 # empty bits

  # Display
  cat("\r Progress: |",
      rep("=", progBits), rep(" ", emptyBits), "| ",
      percentage, "% ...",
      sep = "")

  if (percentage == 100) {cat(" done! \n")}
}

##
fresh <- function(toKeep = "f"){
  #' Clear the console, environment, custom functions, and/or plots.
  #'
  #' @description This function is potentially controversial! However, it helps
  #'   with the way I approach R, whether that is rightly or wrongly. At the
  #'   start of a script / notebook for example, it can be helpful to clear
  #'   everything and reset to see that things are working and not relying on
  #'   something you didn't realise was in the memory. It is not the same as
  #'   restarting R or closing RStudio, but I haven't run into an instance where
  #'   it is actually an issue.
  #'
  #' @param toKeep "string": Which of the four options should be kept?
  #'   * f    functions
  #'   * v    variables/values
  #'   * p    plots
  #'   * c    console
  #'   * e    environment (i.e. functions & variables)
  #'
  #' @examples
  #' \dontrun{
  #'   # Clear the variables, plots, and console, but keep functions:
  #'   fresh()     # default behaviour
  #'
  #'   # Clear the environment and console, but keep the plots:
  #'   fresh("p")
  #'
  #'   # Clear everything
  #'   fresh("")
  #' }
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  # If e (environment), we want v & f
  if (grepl("e", toKeep)) {
    print(toKeep)
    toKeep <- paste0(toKeep, "vf")
    print(toKeep)
  }

  # Keep functions?
  if (!grepl("f", toKeep)) {
    print("Removing functions")
    # if no f, remove functions
    toRemove <- utils::lsf.str(envir = globalenv())
    rm(list = toRemove, envir = globalenv())
  } else {
    print("Keeping functions")
  }

  # Keep variables?
  if (!grepl("v", toKeep)) {
    # if no e, remove non-functions
    print("Removing non-functions")

    # List everything in the global environment
    envAll       <- ls(envir = globalenv())

    # List functions
    envFunctions <- utils::lsf.str(envir = globalenv())

    # Identify non functions
    envNonFuncts <- envAll %in% envFunctions

    # List non-functions to remove
    toRemove <- envAll[!envNonFuncts]

    # Removing non-functions
    rm(list = toRemove, envir = globalenv())
  } else {
    print("Keeping non-functinos")
  }

  # Keep plots?
  if (!grepl("p", toKeep)) {
    print("Removing plots")
    if (length(grDevices::dev.list()) > 0) {
      sapply(grDevices::dev.list(), grDevices::dev.off)
    }
  } else {
    print("Keeping Plots")
  }

  # Keep or clear console?
  if (!grepl("c", toKeep)) {
    cat("\014")
  } else {
    print("Keeping Console")
    print_line()
  }
}

# Uncomment to test fresh function
# cat("\014")
# print("heheehihiiiiie")
# u_tt <- "testing"
# plot(1:10)
# fresh("v")

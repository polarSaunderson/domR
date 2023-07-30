create_figure_directory <- function(relativePath = "", text = "inProgress") {
  #' Create a new directory to store today's figures
  #'
  #' @description When creating pdfs or png figures, it is easier to store them
  #'   in a separate directory each day. This function simply checks if a
  #'   directory already exists, creates a correctly named directory if not, and
  #'   returns the path so it can easily be written to.
  #'
  #' @param relativePath "string": Where is the "Figures/" directory (or where
  #'   should it be created if it doesn't already exist?). This directory is the
  #'   parent of the daily subdirectories.
  #' @param text "string": What text should go in the directory name after the
  #'   current date? Defaults to "inProgress".
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  figPath <- paste0(relativePath, "Figures/")
  # Create a "Figures" directory
  if (!dir.exists(figPath)) {
    dir.create(figPath)
  }

  # Create a directory for today's figures
  todaysFigures <- paste0(figPath, domR::now(format = "F"), "_", text, "/")
  if (!dir.exists(todaysFigures)) {
    dir.create(todaysFigures)
  }
  return(todaysFigures)
}

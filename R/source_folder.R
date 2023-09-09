##
source_folder <- function(folderName, verbose = FALSE, showWarnings = TRUE) {
  #' Source every file in a folder
  #'
  #' @description Useful when loading all custom functions in a project for
  #'   example. Simply finds any file with an ".R" extension, and calls it with
  #'   [source()].
  #'
  #'   Almost identical to this Stackoverflow answer:
  #'     [https://stackoverflow.com/a/20083589]()
  #'
  #' @param folderName Where are the files to source?
  #' @param verbose logical. Print details to the console?
  #' @param showWarnings logical. Should a warning be thrown if no files found?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  files <- list.files(folderName, full.names=TRUE)

  # Grab only R files
  rFiles <- files[ grepl("\\.[rR]$", files) ]

  # Warn if no R files
  if (!length(rFiles) && showWarnings)
    warning("No R files in ", folderName)

  # Source files
  for (f in rFiles) {
    if (verbose) cat("sourcing: ", f, "\n")                  # tell the console
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose) # source, complex
  }
  return(invisible(NULL))
}

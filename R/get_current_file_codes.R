get_current_file_codes <- function(addGit = TRUE) {
  #' Return the file and project codes for the current file
  #'
  #' @description Sometimes when we print to a ".qmd" notebook, save some data,
  #'   or save a figure, it can be helpful to know exactly where it came from.
  #'   My system works on codes at the beginning of file and project names. The
  #'   idea of this function is therefore to quickly grab those codes, based on
  #'   the file where this function is called from. This function assumes that
  #'   the codes follow my own project and file name conventions:
  #'     * project codes      xxx00_project_name
  #'     * file code          yy00_file_name.ext
  #'
  #'   The function searches for any parts of the filename that contain a "_",
  #'   splits it up around those "_", and then assumes that the first ones are
  #'   interesting codes for us. We can optionally add the 7-digit SHA of the
  #'   last git commit to add some version control info if `git2r` is installed.
  #'   The function only works in RStudio as far as I can tell, because it uses
  #'   `rstudioapi::getSourceEditorContext()`. It is still a work-in-progress
  #'   and needs some improvement!
  #'
  #' @param addGit BINARY: Should the 7-digit SHA from the last git commit be
  #'   appended onto the file and project codes? Requires the git2r package.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # File path
  filePath  <- rstudioapi::getSourceEditorContext()$path
  fileParts <- strsplit(filePath, "/", TRUE) |> unlist()

  # File & project codes
  fileCodes <- fileParts[grepl("_", fileParts)]
  returnCodes <- c()
  for (ii in fileCodes) {
    returnCodes[ii] <- strsplit(ii, "_")[[1]][[1]]
  }
  returnCodes <- paste(returnCodes, collapse = "_")

  # Last git SHA
  if (isTRUE(addGit)) {
    lastGitCommit <- git2r::last_commit()$sha |>
      substring(1, 7)
    returnCodes <- paste(returnCodes, lastGitCommit, sep = "_")
  }
  return(returnCodes)
}

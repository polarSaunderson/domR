add_metadata <- function(filePath,
                         exifPath = "../../../exiftool-12.65/exiftool.exe") {
  #' Add metadata keywords to a file using exifTools
  #'
  #' @description A way to document which params were used for a figure, and to
  #'   track the file and latest git commit. Only tested with the windows
  #'   executable.
  #'
  #' @param filePath Which file to add metadata to?
  #' @param exifPath The filepath to the ExifTools executable.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # The current file code and latest git commit
  keywords <- paste0("-Keywords=", domR::get_current_file_codes())

  # Params
  paramNames <- names(params)
  for (ii in seq_along(params)) {
    iiParam  <- paste0("-Keywords=",
                       paste(paste(paramNames[ii])))
    iiData   <- paste0(params[[ii]], collapse = ",")
    iiBit    <- paste(iiParam, iiData, sep = ":")
    keywords <- paste(keywords, iiBit)
  }

  # Add the metadata
  system2(exPath, args = c(kPair, filePath))
}

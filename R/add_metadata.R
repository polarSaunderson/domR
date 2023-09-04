add_metadata <- function(filePath,
                         ...,
                         exifPath  = "../../../exiftool-12.65/exiftool.exe",
                         addToFile = TRUE) {
  #' Add metadata keywords to a file using exifTools
  #'
  #' @description A way to document which params were used for a figure, and to
  #'   track the file and latest git commit. Only tested with the windows
  #'   executable.
  #'
  #' @param filePath Which file to add metadata to?
  #' @param ... Anything else to add as keywords beyond the the params and file
  #'   code. Include as "name" = value.
  #' @param exifPath The filepath to the ExifTools executable.
  #' @param addToFile BINARY: If FALSE, the keywords are invisibly returned, and
  #'   the call to exifTools is skipped.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Current file code and latest git commit
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

  # Anything else?
  dots <- list(...)
  if (length(dots > 0)) {
    dotNames   <- names(dots)
    for (ii in seq_along(dots)) {
      iiDot    <- paste0("-Keywords=",
                         paste(paste(dotNames[ii])))
      iiData   <- paste0(dots[[ii]], collapse = ",")
      iiBit    <- paste(iiDot, iiData, sep = ":")
      keywords <- paste(keywords, iiBit)
    }
  }

  # Add the metadata
  if (isTRUE(addToFile)) {
    system2(exifPath, args = c(keywords, "-overwrite_original", filePath))
  }
  return(invisible(keywords))
}

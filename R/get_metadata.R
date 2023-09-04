get_metadata <- function(format = "named", ...,
                         incGit = TRUE,
                         incSession = TRUE) {
  #' Prepare metadata about a file
  #'
  #' @description Get the information necessary for documenting how a file /
  #'   dataset was created from a .qmd notebook. It only works within a .qmd
  #'   notebook as it directly looks for the 'params' defined at the top.
  #'
  #' @param format Which format do you want the metadata in? Options are "pdf",
  #'   "NetCDF", "print", "yaml" or "json" or "list" (all the same), or "named".
  #'   "named" is the default and the fallback if anything except one of these
  #'   strings is entered (cASe inSENSItIve).
  #'
  #'   Options (examples of formatting):
  #'
  #'      "pdf"                      "-Keywords=month:Dec"
  #'      "NetCDF"                   "month=Dec"
  #'      "print"                    "month : Dec"
  #'      "yaml" / "json" / "list"   list("month" = "Dec")
  #'      "named"                    "month" = "Dec"
  #'
  #' @param ... Any additional information that needs to be included beyond the
  #'   params and file code? Include as '"name" =  value'.
  #' @param incGit BINARY: Should the full output from [get_latest_git()] be
  #'   included? Even if FALSE, the hash of the last commit is still included.
  #' @param incSession BINARY: Should the output from [sessionInfo()] be
  #'   included? This is currently not implemented as it looks more complex than
  #'   I had anticipated.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Preallocate
  metadata     <- paste0("Metadate=", now("n"))

  # Current file code (of the file that get_metadata was initially called from)
  fileCodes    <- paste0("fileCode=", get_current_file_codes())
  metadata     <- c(metadata, fileCodes)

  # Params from the .qmd
  if (exists("params")) {
    paramNames  <- names(params)
    paramValues <- params |> unlist() |> unname()
    paramPairs  <- paste(paramNames, paramValues, sep = "=")
    metadata <- c(metadata, paramPairs)
  }

  # Anything else?
  dots        <- list(...)
  if (length(dots) > 0) {
    dotNames  <- names(dots)
    dotValues <- dots |> unlist() |> unname()
    dotPairs  <- paste(dotNames, dotValues, sep = "=")
    metadata <- c(metadata, dotPairs)  # Combine
  }

  # Add git?
  if (isTRUE(incGit)) {
    git       <- get_latest_git(FALSE, FALSE)
    gitNames  <- gsub(pattern = ":", replacement = "", x = names(git))
    gitValues <- git |> unlist() |> unname()
    gitPairs  <- paste(gitNames, gitValues, sep = "=")
    metadata <- c(metadata, gitPairs)
  }

  # Add sessionInfo?
  if (isTRUE(incSession)) {
    # sesh     <- sessionInfo() # |> capture.output()
  }

  # Format ---------------------------------------------------------------------
  if (tolower(format) == "pdf") {
    metadata <- gsub(pattern = "=", replacement = ":", x = metadata)
    metadata <- paste0("-Keywords=", metadata)
  } else if (tolower(format) == "NetCDF") {
    metadata <- metadata
  } else if (tolower(format) == "print") {
    metadata <- gsub(pattern = "=", replacement = " : ", x = metadata)
  } else {
    # split   <- strsplit(x = metadata, split = "=")
    namesBit <- strsplit(metadata, "=") |> lapply('[[', 1) |> unlist()
    valueBit <- strsplit(metadata, "=") |> lapply('[[', 2) |> unlist()
    metadata <- valueBit |> `names<-`(namesBit)
# names(infoBit) <- nameBit
# setNames(object = infoBit, nm = nameBit)

    # Format as list if necessary
    if (tolower(format) %in% c("list", "yaml", "json")) {
      metadata <- as.list(metadata)
    }
  }

  return(metadata)
}

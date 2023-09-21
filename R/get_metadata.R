get_metadata <- function(...,
                         blurb = NULL,
                         format = "list",
                         incGit = TRUE,
                         incSession = TRUE) {
  #' Prepare metadata about a file
  #'
  #' @description Get the information necessary for documenting how a file /
  #'   dataset was created. It includes a time and date for when it was called,
  #'   the filecode (see [domR::get_file_codes()]) of  the file it was called f
  #'   rom; any 'params' if called from a .qmd file, and (optionally)
  #'   information on the last git commit (requires
  #'   [git2r](https://docs.ropensci.org/git2r/)); the output from
  #'   [sessionInfo()]; and any additional information that is entered as named
  #'   arguments (e.g. '"extraMetadata" = "useful information"').
  #'
  #' @param ... Any additional information that needs to be included beyond the
  #'   params and file code? Include as `"name" =  value`.
  #' @param blurb An blurb / summary / overview of the list being saved. Leave
  #'   as NULL to ignore.
  #' @param format Which format do you want the metadata in? Options are "pdf",
  #'   "NetCDF", "print", "named", or "yaml" / "json" / "list" (all the same).
  #'   "list" is the default and the fallback if anything except one of these
  #'   strings is entered.
  #'
  #'   Options (examples of formatting); names are cASe inSENSItIve.
  #'
  #'      "pdf"                      "-Keywords=month:Dec"
  #'      "NetCDF"                   "month=Dec"
  #'      "print"                    "month : Dec"
  #'      "named"                    named vector: "month" = "Dec"
  #'      "yaml" / "json" / "list"   list("month" = "Dec")
  #' @param incGit BINARY: Should the full output from [domR::get_latest_git()]
  #'   be included? Even if FALSE, the hash of the last commit is still included
  #'   in the file code. Only works if `git2r` is installed.
  #' @param incSession BINARY: Should the output from [sessionInfo()] be
  #'   included? This is formatted and simplified, and includes only the bits
  #'   that I think are interesting to keep. Uses the [domR::get_sessionInfo()]
  #'   function.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Preallocate
  metadata     <- paste0("MetadateCreation<-<-<=>->->", now("n"))

  # Current file code (of the file that get_metadata was initially called from)
  fileCodes    <- paste0("fileCode<-<-<=>->->", get_file_codes())
  metadata     <- c(metadata, fileCodes)

  # Blurb
  if (!is.null(blurb)) {
    blurbPair <- paste("blurb", blurb, sep = "<-<-<=>->->")
    metadata  <- c(metadata, blurbPair)
  }

  # Params from the .qmd
  if (exists("params")) {
    paramNames  <- paste0("params_", names(params))
    paramValues <- params
    paramPairs  <- paste(paramNames, paramValues, sep = "<-<-<=>->->")
    metadata <- c(metadata, paramPairs)
  }

  # Anything else?
  dots <- list(...)
  if (length(dots) > 0) {
    dotNames  <- names(dots)
    dotValues <- dots #|> unlist() |> unname()
    dotPairs  <- paste(dotNames, dotValues, sep = "<-<-<=>->->")
    metadata <- c(metadata, dotPairs)  # Combine
  }

  # Add git?
  if (isTRUE(incGit)) {
    git       <- get_latest_git(FALSE, FALSE)
    gitNames  <- gsub(pattern = ":", replacement = "", x = names(git))
    gitNames  <- paste0("git_", gitNames)
    gitValues <- git # |> unlist() |> unname()
    gitPairs  <- paste(gitNames, gitValues, sep = "<-<-<=>->->")
    metadata <- c(metadata, gitPairs)
  }

  # Add sessionInfo?
  if (isTRUE(incSession)) {
    seshData   <- get_sessionInfo()
    seshNames  <- names(seshData)
    seshPairs  <- paste(seshNames, seshData, sep = "<-<-<=>->->")
    metadata  <- c(metadata, seshPairs)
  }

  # Format ---------------------------------------------------------------------
  if (tolower(format) == "pdf") {
    metadata <- gsub(pattern = "<-<-<=>->->", replacement = ":", x = metadata)
    metadata <- paste0("-Keywords=", metadata)
  } else if (tolower(format) %in% c("netcdf", "nc")) {
    metadata <- gsub(pattern = "<-<-<=>->->", replacement = "=", x = metadata)
  } else if (tolower(format) == "print") {
    metadata <- gsub(pattern = "<-<-<=>->->", replacement = " : ", x = metadata)
  } else {
    namesBit <- strsplit(metadata, "<-<-<=>->->") |> lapply('[[', 1) |> unlist()
    valueBit <- strsplit(metadata, "<-<-<=>->->") |> lapply('[[', 2) |> unlist()
    metadata <- valueBit |> `names<-`(namesBit)
    if (tolower(format) == "named") {
      return(metadata)
    } else {
    # Otherwise, format as list
      if (tolower(format) %in% c("list", "yaml", "json")) {
        metadata <- as.list(metadata)
      }
    }
  }
  return(metadata)
}

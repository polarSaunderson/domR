which_line <- function(skipCatCall = TRUE) {
  #' Print out the line number/s a function is called on
  #'
  #' @description This function is not complete, but it is getting somewhere...
  #'   Imagine when debugging, but without actual errors. You're just trying to
  #'   find out the value of a variable at different stages. We want to know
  #'   which line of a file that is on. This is therefore essentially a wrapper
  #'   around using `.traceback(x)` when we haven't an actual error to
  #'   traceback.
  #'
  #'   This function is mainly designed to be used inside the [cat4()] function.
  #'   [cat4()] is a combination of [cat2()] and [which_line()], so it tells the
  #'   user the variable name, the variable value, and what the location.
  #'
  #'   **IMPORTANT** This function (and therefore [cat4()] is only designed to
  #'   be used interactively in a "qmd" or "rmd" notebook.
  #'
  #'   **IMPORTANT** The line identification of the call file **WILL** be
  #'   incorrect if:
  #'     the *exact* same call is used in different chunk before the chunk used.
  #'
  #'   For identifying the current file it is called in, it requires the
  #'   `rstudioapi` package (i.e. it only works with that, so not pure R) to
  #'   identify the document and save it.
  #'
  #'   The code was inspired by this stackoverflow question:
  #'     [https://stackoverflow.com/questions/59537482/how-to-get-line-number-of-a-function-call-in-r]().
  #'
  #' @param skipCatCall When nested in another function (for example [cat4()]),
  #'   should the call to that function (e.g.`cat4(x)`) be included in the print
  #'   out of where [which_line()] was called from? TRUE by default.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Get some information about the last calls
  if (isTRUE(skipCatCall)) {
    functionList <- .traceback(2)
  } else {
    functionList <- .traceback(1)
  }

  # Which line were the calls made on?
  for (ii in seq_along(functionList)) {
    if (ii > 1) {
      iiLine <- attr(functionList[[ii - 1]], "srcref")[[1]]
      if (ii == 2) {
        lineBit <- "On line "
      } else {
        lineBit <- "called from line "
      }
      cat("\n-- ", paste0(rep("-- ", ii - 2)),
          lineBit,
          paste(iiLine, "of", functionList[[ii]]), sep = "")
    }
  }

  if (length(functionList) == 0) ii <- 2 # for counting indents later

  # File -----------------------------------------------------------------------
  # This only partially works, because it doesn't account for the same function
  # appearing in an earlier chunk than the chunk being run.

  # Which file was the function called in?
  filePath  <- rstudioapi::getSourceEditorContext()$path
  rstudioapi::documentSave()
  fileName  <- basename(filePath)

  # Which line/s of the file is the function call found on?
  fileText  <- readLines(filePath)
  functionName <- functionList[[length(functionList)]]
  fileLines <- grep(pattern = functionName, x = fileText, fixed = TRUE)

  # If the function call occurs multiple times, we want to know which call is
  # which. This is the part that can be tripped up by not knowing which chunk
  # it is being called from - we track how many times the call is made in the
  # chunk, but compare it against how many times the call appears overall.
  # I haven't figured out a way to identify the chunk yet.

  # Tracking which call to the function it is
  if (!exists("whichLine_info")) {
    cat3(show = FALSE, "\ncreating new info")
    .GlobalEnv$whichLine_info <- list()
  } else { cat3(show = FALSE, "whichLine_info already exists") }

  # Tracking the file & function
  # We track file, though I can't remember the use case that made me do it.
  # As far as I can tell, it will always go to the same file, where we initially
  # ran it from - everything else is a function call, and I don't think it works
  # with `source()`.
  if (fileName %in% names(.GlobalEnv$whichLine_info)) {
    cat3(show = FALSE, "file already being tracked")
    cat2(show = FALSE, names(.GlobalEnv$whichLine_info[[fileName]]))

    if (functionName %in% names(.GlobalEnv$whichLine_info[[fileName]])) {
      cat3(show = FALSE, "function already being tracked, adding 1")
      .GlobalEnv$whichLine_info[[fileName]][[functionName]] <- .GlobalEnv$whichLine_info[[fileName]][[functionName]] + 1
    } else {
      cat3(show = FALSE, "function not being tracked, creating 1")
      .GlobalEnv$whichLine_info[[fileName]][[functionName]] <- 1
    }
  } else {
    cat3(show = FALSE, "file not being tracked yet, creating it, & function, and adding 1")
    .GlobalEnv$whichLine_info[[fileName]][[functionName]] <- 1
  }

  # Get the index - this is the whole point!
  fileLineIndex <- .GlobalEnv$whichLine_info[[fileName]][[functionName]]

  # Tidy up .GlobalEnv
  ## 1) check if the function has been finished with
  if (.GlobalEnv$whichLine_info[[fileName]][[functionName]] == length(fileLines)) {
    # cat_list(.GlobalEnv$whichLine_info)
    cat3(show = FALSE, "going to NULL function tracking")
    .GlobalEnv$whichLine_info[[fileName]][[functionName]] <- NULL
    cat3(show = FALSE, "nulled function tracking")
    # cat_list(.GlobalEnv$whichLine_info)
  }

  ## 2) check if the file has been finished with
  if (length(.GlobalEnv$whichLine_info[[fileName]]) == 0) {
    cat3(show = FALSE, "going to NULL file tracking")
    .GlobalEnv$whichLine_info[[fileName]] <- NULL
    cat3(show = FALSE, "nulled file tracking")
  }

  ## 3) check if the tracking has been finished with
  if (length(.GlobalEnv$whichLine_info) == 0) {
    cat3(show = FALSE, "removing all tracking")
    rm(whichLine_info, envir = .GlobalEnv)
  }

  # Display file information
  if (length(functionList) == 1) {
    lineBit <- "On line"
  } else {
    lineBit <- "called from line"
  }

  cat("\n--", paste0(rep("--", ii - 1)),
      lineBit, fileLines[fileLineIndex],
      "of", fileName, "\n")
}


  #   # print("adding 1")
  #   .GlobalEnv$whichLine_info[[fileName]] <- .GlobalEnv$whichLine_info[[fileName]] + 1
  # } else {
  #   # print("creating 1")
  #   .GlobalEnv$whichLine_info[[fileName]] <- 1
  # }
  #
  # fileLineIndex <- .GlobalEnv$whichLine_info[[fileName]]
  # # print(fileLineIndex)
  #
  # if (.GlobalEnv$whichLine_info[[fileName]] == length(fileLines)) {
  #   # print("removing")
  #   rm(whichLine_info, envir = .GlobalEnv)
  # }
  #
  #
  #
  # if (length(functionList) == 0) {
  #   cat("\n-- ", #rep("-- ", length(functionList)),
  #       "called on line", fileLines[[fileLineIndex]],
  #       "of", fileName, "\n")
  # }
# }

  #
  #
  #
  #
  # Which functions have been called?
  # x <- .traceback(x = 1)
  # functionList <- x
  # cat2(functionList)
  #
  # # Which line of the function/s are we on?
  # funcLines <- attr(x[[1]], "srcref")
  # cat2(funcLines)
  #
  # functionList[[1]] <- NULL          # ignore the which_line function call
  # cat2(functionList)
  # # funcLines <- funcLines[-1]
  # # funcLines[[1]][1] <- NULL          # ignore the which_line function call
  # cat2(funcLines)
  # if (isTRUE(skipInNested)) {
  # # if (functionName != "which_fun") { # ignore function nesting which_fun
  #   functionList[[1]] <- NULL
  #   funcLines <- funcLines[-1]
  # }
  #
  # cat2(functionList)
  # cat2(funcLines)
  #
  # # # Display
  # if (length(functionList) == 0) {
  #   cat("-- On line", fileLines[[fileLineIndex]], "of", fileName)
  # } else {
  #   for (ii in seq_along(functionList)) {
  #     cat("\n-- ", paste(rep("-- ", ii), collapse = ""),
  #         "called on line ",
  #         paste(funcLines[[ii]], "of", functionList[[ii]]), sep = "")
  #   }
  #   cat("\n-- called on line", fileLines[[fileLineIndex]], "of", fileName)
  # }
  # cat("\n")

  # # Display
  # cat("-- On line", fileLines[[fileLineIndex]], "of", fileName)
  # for (ii in seq_along(functionList)) {
  #   cat("\n-- ", paste(rep("-- ", ii), collapse = ""),
  #       "called on line ",
  #       paste(funcLines[[ii]], "of", functionList[[ii]]), sep = "")
  # }
  # cat("\n")
# }

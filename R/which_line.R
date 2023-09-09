which_line <- function(skipCatCall = TRUE) {
  #' Print out the line number/s a function is called on
  #'
  #' @description This function is experimental, but is getting somewhere...
  #'   Imagine when debugging, but without actual errors. You're just trying to
  #'   find out the value of a variable at different stages. It is necessary to
  #'   know which line of a file that is on. This is therefore essentially a
  #'   wrapper around using `.traceback(x)` when we haven't an actual error to
  #'   traceback.
  #'
  #'   This function is mainly designed to be used inside the [cat4()] function.
  #'   [cat4()] is a combination of [cat2()] and [which_line()], so it tells the
  #'   user the variable name, the variable value, and what the location.
  #'
  #'   **IMPORTANT** This function (and therefore [cat4()] is only designed to
  #'   be used interactively in a "qmd" or "rmd" notebook. Untested with
  #'   `source()`.
  #'
  #'   **IMPORTANT** The line identification requires that the cursor is in the
  #'   chunk being called. It is based on the `rstudioapi` package, so doesn't
  #'   work with pure R.
  #'
  #'   The code was inspired by this stackoverflow question:
  #'   [https://stackoverflow.com/questions/59537482/how-to-get-line-number-of-a-function-call-in-r]().
  #'
  #'
  #' @param skipCatCall When nested in another function (for example [cat4()]),
  #'   should the call to that function (e.g.`cat4(x)`) be included in the print
  #'   out of where [which_line()] was called from? TRUE by default.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # for debugging
  showCat <- FALSE

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
  # This only partially works, because it requires that the cursor was in the
  # chunk being run. It defines the chunk by looking for "```", so a script may
  # still work if those are not present.

  # Which function is being checked? The full call (e.g. `cat4(x)`)
  functionName <- functionList[[length(functionList)]]

  # Which file was the function called in?
  srcInfo    <- rstudioapi::getSourceEditorContext()
  fileText   <- srcInfo$contents
  cursorLine <- srcInfo$selection[[1]]$range$start[["row"]]
  filePath   <- srcInfo$path
  fileName   <- basename(filePath)

  # Extract only text for the current chunk
  chunkBreaks <- which(grepl(pattern = "```", x = fileText))
  if (length(chunkBreaks) != 0) {
    chunkStart <- chunkBreaks[max(which(chunkBreaks <= cursorLine))]
    chunkEnd   <- chunkBreaks[min(which(chunkBreaks >= cursorLine))]
    chunkText  <- fileText[chunkStart:chunkEnd]
  } else {
    chunkText  <- fileText  # quick attempt at making it viable for scripts
  }

  # Handle if the function is present in the chunk but commented out
  hashLines <- gregexpr(pattern = "#", text = chunkText,
                        fixed = TRUE) |> unlist()
  hashLines[hashLines < 0] <- NA  # NA index of lines without a hash

  funcLines <- gregexpr(pattern = functionName, text = chunkText,
                        fixed = TRUE) |> unlist()
  funcLines[funcLines < 0] <- NA  # NA index of lines without the function

  # Ignore the commented lines, but keep as important for the order
  commentLines <- which(hashLines < funcLines) # comment is before function
  chunkText[commentLines] <- ""

  # Which line/s of the file is the function call found on?
  fileLines    <- grep(pattern = functionName, x = chunkText, fixed = TRUE)

  # If the function call occurs multiple times, we want to know which call is
  # which. This is the part that can be tripped up by not knowing which chunk
  # it is being called from - we track how many times the call is made in the
  # chunk, but compare it against how many times the call appears overall.
  # I haven't figured out a way to identify the chunk yet.

  # Tracking which call to the function it is
  # if (!exists("whichLine_info")) {
  if (!exists("whichEnv", where = globalenv())) {
    cat3(show = showCat, "\nCreating new environment: whichEnv.")
    whichEnv <- new.env()
    assign("whichEnv", whichEnv, envir = globalenv())
    whichEnv$line_info <- list()
  # } else { cat3(show = FALSE, "whichLine_info already exists") }
  } else { cat3(show = showCat, "The whichEnv environment already exists!") }

  # Tracking the file & function
  # We track file, though I can't remember the use case that made me do it.
  # As far as I can tell, it will always go to the same file, where we initially
  # ran it from - everything else is a function call, and I don't think it works
  # with `source()`.
  if (fileName %in% names(whichEnv$line_info)) {
    cat3(show = showCat, "'", fileName,
         "' (filename) is already being tracked in whichEnv.")

    if (functionName %in% names(whichEnv$line_info[[fileName]])) {
      cat3(show = showCat, "'", functionName,
           "' (function) is already being tracked; adding 1 to the counter.")
      whichEnv$line_info[[fileName]][[functionName]] <- whichEnv$line_info[[fileName]][[functionName]] + 1
    } else {
      cat3(show = showCat, "'", functionName,
           "' (function) is not being tracked; creating a counter.")
      whichEnv$line_info[[fileName]][[functionName]] <- 1
    }
  } else {
    cat3(show = showCat, "'", fileName,
         "' (filename) not being tracked yet; creating a counter.")
    whichEnv$line_info[[fileName]][[functionName]] <- 1
  }

  # Get the index - this is the whole point!
  fileLineIndex <- whichEnv$line_info[[fileName]][[functionName]]

  # Tidy up .GlobalEnv
  ## 1) check if the function has been finished with
  if (whichEnv$line_info[[fileName]][[functionName]] == length(fileLines)) {
    # cat_list(whichEnv$line_info)
    cat3(show = showCat, "'", functionName,
         "' (function) finished with; NULLing function counter...")
    whichEnv$line_info[[fileName]][[functionName]] <- NULL
    cat3(show = showCat, "'", functionName,
         "' (function) counter NULLED.")
    # cat_list(whichEnv$line_info)
  } else {
    cat3(show = showCat, "'", functionName,
         "' (function) still active.")
  }

  ## 2) check if the file has been finished with
  if (length(whichEnv$line_info[[fileName]]) == 0) {
    cat3(show = showCat, "'", fileName,
         "' (filename) finished with; NULLing file counter...")
    whichEnv$line_info[[fileName]] <- NULL
    cat3(show = showCat, "'", fileName,
         "' (filename) counter nulled.")
  } else {
    cat3(show = showCat, "'", fileName,
         "' (filename) still active.")
  }

  ## 3) check if the tracking has been finished with
  if (length(whichEnv$line_info) == 0) {
    cat3(show = showCat,
         "Finished all tracking, removing whichEnv...")
    # cat3(show = showCat, "printing .GlobalEnv:")
    # cat_list(.GlobalEnv)
    rm(whichEnv, envir = .GlobalEnv)
    cat3(show = showCat,
         "whichEnv removed from the global environment!")
    # cat_list(.GlobalEnv)
    # cat3(show = showCat, "the above shouldn't have a whichEnv in...")
  } else {
    cat3(show = showCat,
         "whichEnv is still actively tracking function calls")
  }

  # Display file information ---------------------------------------------------
  if (length(functionList) == 1) {
    lineBit <- "On line"
  } else {
    lineBit <- "called from line"
  }

  cat("\n--", paste0(rep("--", ii - 1)),
      lineBit, fileLines[fileLineIndex] + chunkStart - 1,
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

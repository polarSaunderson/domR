which_line <- function(functionName = "which_line", skipInNested = TRUE) {
  #' Print out the line number/s a function is called on
  #'
  #' @description This function is not complete, but it is getting somewhere...
  #'   Imagine when debugging, but without actual errors. You're just trying to
  #'   find out the value of a variable at different stages. We want to know
  #'   which line of a file that is on.
  #'
  #'   This function is mainly designed to be used inside the [cat4()] function.
  #'   That function is a combination of [cat2()] and [which_line()], so it
  #'   tells us what the variable name is, the value, and what line it was
  #'   printed on.
  #'
  #'   It requires rstudioapi (i.e. it only works with that, so not pure R).
  #'   This is to identify the document  and save it. It then uses
  #'
  #'   The code was inspired by this stackoverflow question:
  #'     [https://stackoverflow.com/questions/59537482/how-to-get-line-number-of-a-function-call-in-r]().
  #'
  #' @param functionName The text to search for in the file; if not
  #'   [which_line()] (the default), it should match the name of the parents
  #'   function that it is being called in.
  #' @param skipInNested When nested in another function (for example [cat4()]),
  #'   should the call to that function (e.g.[cat4()]) be included in the print
  #'   out of where [which_line()] was called from? TRUE by default.
  #'
  #' @export

  # Code -----------------------------------------------------------------------

  # Which file was the function called in?
  filePath  <- rstudioapi::getSourceEditorContext()$path
  rstudioapi::documentSave()
  fileName  <- basename(filePath)

  # Which line of the file is this found on?
  fileText  <- readLines(filePath)
  fileLines <- which(grepl(functionName, fileText))
  # cat2(fileLines)

  # Tracking which one it is
  if (!exists("whichLine_info")) {
    # print("creating new info")
    .GlobalEnv$whichLine_info <- list()
  } # else { print("it already exists") }

  if (fileName %in% names(.GlobalEnv$whichLine_info)) {
    # print("adding 1")
    .GlobalEnv$whichLine_info[[fileName]] <- .GlobalEnv$whichLine_info[[fileName]] + 1
  } else {
    # print("creating 1")
    .GlobalEnv$whichLine_info[[fileName]] <- 1
  }

  fileLineIndex <- .GlobalEnv$whichLine_info[[fileName]]
  # print(fileLineIndex)

  if (.GlobalEnv$whichLine_info[[fileName]] == length(fileLines)) {
    # print("removing")
    rm(whichLine_info, envir = .GlobalEnv)
  }

  # Get some information about the last calls
  functionList <- .traceback(1)
  # cat2(functionList)

  # Which line?
  # for (ii in seq_along(functionList)[-length(functionList)]) {
  for (ii in seq_along(functionList)) {
    # print(ii)
    if (ii > 1) {#< length(functionList)) {
      iiLine <- attr(functionList[[ii - 1]], "srcref")[[1]]
    # } else {
      # iiLine <- ""
    # }
    # cat2(iiLine)
    cat("\n-- ", paste(rep("-- ", ii - 2), collapse = ""),
        "called on line ",
        paste(iiLine, "of", functionList[[ii]]), sep = "")
    }
  }

  if (length(functionList) == 0) {
    cat("\n-- ", #rep("-- ", length(functionList)),
        "called on line", fileLines[[fileLineIndex]],
        "of", fileName, "\n")
  }
}

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

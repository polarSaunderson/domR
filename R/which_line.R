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
  # Get some information about the last calls
  x <- .traceback(x = 1)

  # Which file was the function called in?
  filePath  <- rstudioapi::getSourceEditorContext()$path
  rstudioapi::documentSave()
  fileName  <- basename(filePath)

  # Which line of the file is this found on?
  fileText  <- readLines(filePath)
  fileLines <- which(grepl(functionName, fileText))

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

  # Which functions have been called?
  functionList <- x
  functionList[[1]] <- NULL          # ignore the which_line function call
  if (isTRUE(skipInNested)) {
  # if (functionName != "which_fun") { # ignore function nesting which_fun
    functionList[[1]] <- NULL
  }

  # Which line of the function/s are we on?
  funcLines <- attr(x[[1]], "srcref")

  # Display
  cat("-- On line", fileLines[[fileLineIndex]], "of", fileName)
  for (ii in seq_along(functionList)) {
    cat("\n-- ", paste(rep("-- ", ii), collapse = ""),
        "called on line ",
        paste(funcLines[[ii]], "of", functionList[[ii]]), sep = "")
  }
  cat("\n")
}

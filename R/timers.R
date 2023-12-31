start_timer <- function(...) {
  #' Starts the timer
  #'
  #' @description This function and [end_timer()] are mainly because I can never
  #'   remember the syntax/name of [proc.time()]. Simply call [start_timer()]
  #'   before the process you are timing begins, and then call [end_timer()]
  #'   afterwards, feeding it with the output of [start_timer()]. See examples.
  #'
  #'   These functions will be developed in future versions to allow multiple
  #'   levels of timing.
  #'
  #' @param ... "string" Name of the process that is being timed; all arguments
  #'   are displayed in the order entered using [cat3()].
  #' @examples
  #' \dontrun{
  #'   start <- start_timer("Slow code")
  #'   Sys.sleep(0.2) # do code!
  #'   end_timer(start, "Slow code")
  #' }
  #' @export

  # Code -----------------------------------------------------------------------
  cat3(">>>", ...,                                   # print process name
       "started at:", now("t"), "...", nStart = 0)   # and current time
  return(proc.time())
}

end_timer <- function(startTime, ..., sound = TRUE){
  #' Ends the timer
  #'
  #' @description This function and [start_timer()] are mainly because I can
  #'   never remember the syntax/name of [proc.time()]. Simply call
  #'   [start_timer()] before the process you are timing begins, and then call
  #'   [end_timer()] afterwards, feeding it with the output of [start_timer()].
  #'   This function invisibly returns the time of it finishing, so it can be
  #'   used in a subsequent [end_timer()] function call and the timing of
  #'   processes can be chained together. See the usage examples.
  #'
  #' @param startTime Use the output from [start_timer()] as input here
  #' @param ... "string": Name of the process that has been timed; all arguments
  #'   will be displayed in the order entered using [cat3()].
  #' @param sound BINARY: Should a sound be played upon completion? Requires
  #'    `beepr`.
  #'
  #' @examples
  #' \dontrun{
  #'   start <- start_timer()
  #'   Sys.sleep(0.2) # do code!
  #'   end_timer(start)
  #'
  #'   start2 <- start_timer()
  #'   Sys.sleep(0.2) # do code!
  #'   end_timer(start2, "code block 1", FALSE)
  #'   Sys.sleep(0.8) # do more code!
  #'   end_timer(start2, "code block 2")
  #'
  #'   start3 <- start_timer()
  #'   Sys.sleep(0.2) # do code!
  #'   end1 <- end_timer(start3, "code block 1", FALSE)
  #'
  #'   Sys.sleep(0.6) # do more code!
  #'   end_timer(end1, "code block 2", FALSE)
  #'   end_timer(start, "overall time")
  #' }
  #' @export

  # Code -----------------------------------------------------------------------
  timeTaken <- proc.time() - startTime
  cat3(">>>", ..., "done! It took:", nStart = 0)
  print(timeTaken)
  print_line(nStart = 0)
  if (isTRUE(sound)) {
    if (system.file(package = "beepr") != "") {
      beepr::beep()
    }
  }

  return(invisible(proc.time()))
}

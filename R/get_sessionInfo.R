get_sessionInfo <- function() {
  #' Simplify sessionInfo() for json or yaml
  #'
  #' @description The default output from [sessionInfo()] doesn't play nicely
  #'   when trying to add metadata to a list that is saved in a json or yaml
  #'   format. This function takes the output, extracts the interesting parts
  #'   (very subjectively!) and formats them as a named list. Mainly designed to
  #'   be called by get_metadata as the normal call to [sessionInfo()] is better
  #'   for printing it out.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Get session info
  seshInfo     <- utils::sessionInfo()
  seshCaptured <- utils::capture.output(seshInfo)   # some parts are easier from this

  # Format it
  sessionList <- list() # preallocate

  # Basics - direct from the captured output
  sessionList$Version  <- seshCaptured[which(grepl("R version", seshCaptured))]
  sessionList$Platform <- seshCaptured[which(grepl("Platform:", seshCaptured))]
  sessionList$Running  <- seshCaptured[which(grepl("Running u", seshCaptured))]

  # More complex - reformatting the output
  sessionList$locale    <- get_packages(seshInfo, whichPackages = "locale")
  sessionList$base      <- get_packages(seshInfo, whichPackages = "base")
  sessionList$attached  <- get_packages(seshInfo, whichPackages = "attached")
  sessionList$namespace <- get_packages(seshInfo, whichPackages = "namespace")

  return(sessionList)
}
#   dInfo <- list()
# # dInfo$`R version`       <- seshInfo[which(grepl("R version", seshInfo))]
# dInfo$`R version`       <- seshCaptured[which(grepl("R version", seshCaptured))]
# # dInfo$Platform          <- seshInfo[which(grepl("Platform:", seshInfo))]
# dInfo$Platform          <- seshCaptured[which(grepl("Platform:", seshCaptured))]
# # dInfo$`Running Under`   <- seshInfo[which(grepl("Running under:", seshInfo))]
# dInfo$`Running Under`   <- seshCaptured[which(grepl("Running under:", seshCaptured))]
# dInfo$locale            <- get_packages(seshInfo, whichPackages = "locale")
# dInfo$basePackages      <- get_packages(seshInfo, whichPackages = "base")
# dInfo$attachedPackages  <- get_packages(seshInfo, whichPackages = "attached")
# dInfo$namespacePackages <- get_packages(seshInfo, whichPackages = "namespace")
# }

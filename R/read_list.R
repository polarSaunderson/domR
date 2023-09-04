read_list <- function(filename) {
  #' A very simple wrapper around [jsonlite::read_json()]
  #'
  #' @description This function will be expanded to also accommodate yaml, rds
  #'   and rData output, and different json options. It currently just converts
  #'   a json file to an R list, using a default of `simplifyVector` = TRUE. It
  #'   assumes that it is reading a json file created by [save_list()].
  #'
  #' @param filename The filename and path (relative to the working directory)
  #'   of the list.
  #' @export

  # Code -----------------------------------------------------------------------
  # Figure out extension to determine progress
  ext <- get_extension(filename)

  # Read if json
  if (tolower(ext) == "json") {
    jsonlite::read_json(filename, simplifyVector = TRUE)
  } else {
    stop("Currently only works for .json files storing R lists!")
  }
}

#' read_list <- function(fileName) {
#'   #' Read a .json, .yaml, .rds or .rData list
#'   #'
#'   #' @description This function reads in a list (inc. data frames and
#'   #'   environments).
#'   #'
#'   #' @param fileName The filename and path (relative to the working directory)
#'   #'   of the list. The extension determines how it is read.
#'   #'
#'   #'   Options are:
#'   #'
#'   #'      .json      [JSON]() file; uses [jsonlite]()
#'   #'      .yaml      [YAML]() file; uses [yaml]()
#'   #'      .yml       as above
#'   #'      .rds       Native R format (binary)
#'   #'      .rData     Native R format (binary)
#'   #'
#'   #' @export
#'
#'   # Code -----------------------------------------------------------------------
#'   # Figure out extension to determine progress
#'   ext <- strsplit(fileName, split = ".", fixed = TRUE)[[1]] |>
#'     (function(x) x[length(x)]) ()
#'
#'   if (tolower(ext) == "json") {
#'     list <- jsonlite::read_json(fileName, simplifyVector = TRUE)
#'   } else {
#'     print("not json")
#'   }
#'
#'   return(list)
#' }

save_list <- function(x, filename) {
  #' A very simple wrapper around [jsonlite::write_json()]
  #'
  #' @description This function will be expanded to also accommodate yaml, rds
  #'   and rData output, and different json options. It currently just converts
  #'   an R list to a json format, and saves it. It uses the default values of
  #'   'pretty' = TRUE, and 'dataframe' = "columns".
  #'
  #' @param x The list to save.
  #' @param filename The filename (and path relative to the working directory)
  #'   indicating where to save the list. Must included the file extension.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Figure out extension to determine progress
  ext <- get_extension(filename)
  # ext <- strsplit(filename, split = ".", fixed = TRUE)[[1]] |>
    # (function(x) x[length(x)]) ()

  if (tolower(ext) == "json") {
    jsonlite::write_json(x = x, path = filename,
                         pretty = TRUE, dataframe = "columns",
                         )
  } else {
    stop("Currently only works for saving R lists as json files!")
  }
}

#' save_list <- function(x, fileName, serialize = FALSE, ...) {
#'   #' Save a list as .json, .yaml, .rds or .rData
#'   #'
#'   #' @description This function converts lists (inc. data frames and
#'   #'   environments) to valid json or yaml format, and then saves them. Lists in
#'   #'   these formats are useful for version control, as they are not binary
#'   #'   files, and are human-readable. For convenience, it is also possible to
#'   #'   save the lists as .rData or .rds files. This is just a wrapper around
#'   #'   the respective functions from the `yaml` and `jsonlite` packages.
#'   #'
#'   #' @param x The list, data frame or environment to save.
#'   #' @param fileName The filename (and path relative to the working directory)
#'   #'   indicating where to save the list. The filename extension determines how
#'   #'   the list is saved.
#'   #'
#'   #'   Options are:
#'   #'
#'   #'      .json      [JSON]() file; uses [jsonlite]()
#'   #'      .yaml      [YAML]() file; uses [yaml]()
#'   #'      .yml       as above
#'   #'      .rds       Native R format (binary)
#'   #'      .rData     Native R format (binary)
#'   #'
#'   #' @param ... Any parameters to pass onto the respective functions that save
#'   #'   the list; see [jsonlite::write_json()], [yaml::write_yaml()],
#'   #'   [saveRDS()], or [save()].
#'   #'
#'   #' @export
#'
#'   # Code -----------------------------------------------------------------------
#'   # Figure out extension to determine progress
#'   ext <- strsplit(fileName, split = ".", fixed = TRUE)[[1]] |>
#'     (function(x) x[length(x)]) ()
#'
#'   if (tolower(ext) == "json") {
#'   # THIS BIT IS SUCH A MESS - inspired the (so far unsuccesful)
#'   `handle_defaults` function.
#'     inDots  <- list(...)
#'     defArgs <- list("pretty" = TRUE, "dataframe" = "columns")
#'     defArgs[names(inDots)] <- inDots
#'     fnArgs <- defArgs
#'     fnArgs$x <- x
#'     fnArgs$path <- fileName
#'
#'     if (isTRUE(serialize)) {
#'       fnArgs$x <- jsonlite::serializeJSON(fnArgs$x, pretty = fnArgs$pretty)
#'     }
#'     do.call(jsonlite::write_json, fnArgs)
#'   } else if (tolower(ext) %in% c("yaml", "yml")) {
#'     yaml::write_yaml(x = x, file = fileName, ...)
#'   } else {
#'     print("not json")
#'   }
#' }

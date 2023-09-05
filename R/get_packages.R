get_packages <- function(seshInfo,
                         whichPackages = "namespace",
                         collapse = " ; ") {
  #' Strip the output of sessionInfo for packages!
  #'
  #' @description The information about packages in the [sessionInfo()] output
  #'   is too complicated to store as json or yaml files. This function simply
  #'   strips out the package names and versions, and formats them together as a
  #'   single string that can be the value in a name = value pair.
  #'
  #'   The intention is to feed this function directly into a metadata call as
  #'   (e.g. `"packages_attached" = get_packages(seshInfo, "attached")`). The
  #'   options are "base" packages, "attached" packages or  packages in the
  #'   "namespace" but not attached (i.e. those called as
  #'   `domR::made_up_function()`). It can also be used to retrieve the
  #'   "locale" part of the [sessionInfo()] output.
  #'
  #'   **NOTE!** This very much relies on the [sessionInfo()] output remaining
  #'   in the same formatting as it has now! It has not been extensively test
  #'   *AT ALL*.
  #'
  #' @param seshInfo If NULL, [sessionInfo()] will be called within the
  #'   function. If doing multiple calls (e.g. one each for "base", "namespace"
  #'   and "attached"), it is better to run `seshInfo <- sessionInfo()`  first
  #'   (before calling this function), and use that as input here.
  #' @param whichPackages Options are "base", "attached", or "namespace". Can be
  #'   input as "b", "a", or "n". Will also work for "locale" (or "l") even
  #'   though those are not packages.
  #' @param collapse "string": All package names in a call are returned in a
  #'   single vector. This argument determines how the package names should be
  #'   separated in the vector. The default is " ; ", so the output looks like
  #'   (e.g.) `kulaR_0.0.1.1 ; domR_0.0.1.3`.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Run session info if necessary
  if (is.null(seshInfo)) {
    seshInfo <- sessionInfo()
  }
  seshInfo <- capture.output(seshInfo)

  # Find lines that the packages are on
  lineLocale     <- which(grepl("locale:", seshInfo))
  lineBase       <- which(grepl("attached base packages:", seshInfo))
  lineAttached   <- which(grepl("other attached packages:", seshInfo))
  lineNamespace  <- which(grepl("loaded via a namespace", seshInfo))

  whichPackages <- tolower(whichPackages)
  if (whichPackages %in% c("locale", "l")) {
    packageLines <- seshInfo[(lineLocale + 1):(lineBase - 2)]
  } else if (whichPackages %in% c("base", "b")) {
    packageLines <- seshInfo[(lineBase + 1):(lineAttached - 2)]
  } else if (whichPackages %in% c("attached", "a")) {
    packageLines <- seshInfo[(lineAttached + 1):(lineNamespace - 2)]
  } else if (whichPackages %in% c("namespace", "n")) {
    packageLines <- seshInfo[(lineNamespace + 1):length(seshInfo)]
  } else {
    stop("Select either 'base', 'namespace', 'attached' or 'locale'",
         "in get_packages()")
  }

  # Format down nicely
  pkgs <- strsplit(packageLines, "]") |>
    # strsplit("]") |>
    lapply("[[", 2) |>
    lapply("[[", 1) |>
    unlist() |>         # now just package names and lots of space
    strsplit(" ") |>
    unlist() |>
    (function(x) x[-which(x == "")])() |>
    paste(collapse = collapse)

  return(pkgs)
}

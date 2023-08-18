get_list_names <- function(list, depth = 0, name = NULL) {
  #' Get the names of all values in nested lists
  #'
  #' @description This function is basically a recursive calling of "names" to
  #'   get the names of everything in nested lists.
  #'
  #' @param list The nested list.
  #' @param name Name to be printed at the top of each list. If NULL (the
  #'   default), it matches the name of the 'list' argument. This is mainly here
  #'   for recursion so the "list" in the loop isn't used as the name.
  #' @param depth Used for recursive calls to track the depth.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  listNames <- names(list)

  # Depth is necessary to get the list name with deparse but not repeat it
  depth <- depth + 1
  if (depth == 1) {
    if (is.null(name)) { # lets us overrule deparse in recursions so != "list"
      name <- deparse(substitute(list))
    }
    listNames  <- c(name, listNames)
  }

  # Add names of lists at this level
  for (ii in listNames) {
    iiList <- list[[ii]]
    if (is.list(iiList)) {
      listNames <- c(listNames,
                     get_list_names(iiList, depth)) # recursive call
    }
  }
  return(listNames)
}

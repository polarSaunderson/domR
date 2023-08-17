get_list_names <- function(list) {
  #' Get the names of all values in nested lists
  #'
  #' @description This function is basically a recursive calling of "names" to
  #'   get the names of everything in nested lists.
  #'
  #' @param list The nested list.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  listNames <- names(list)
  for (ii in listNames) {
    iiList <- list[[ii]]
    if (is.list(iiList)) {
      listNames <- c(listNames, get_list_names(iiList))
    }
  }
  return(listNames)
}

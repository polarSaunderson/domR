count_list_depth <- function(list, depth = 0) {
  #' Count how deep nested lists go
  #'
  #' @description Lists can be nested. This simply counts how many nested levels
  #'   there are. This code is largely copied from Spacedman's stackoverflow
  #'   answer here: [https://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r]().
  #'
  #' @param list The list to counted nested list levels within.
  #' @param depth Used to track the depth in recursive calls.
  #'
  #' @noRd

  # Code -----------------------------------------------------------------------
  if (!is.list(list)) {
    return(depth)
  } else if (is.list(list) && length(list) == 0) {
    return(depth)
  } else {
    return(max(unlist(lapply(list, count_list_depth, depth = depth + 1))))
  }
}

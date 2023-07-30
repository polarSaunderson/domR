get_initials <- function(x, collapse = "") {
  #' Return the initials of a vector of strings
  #'
  #' @description This function is especially helpful when we are using months,
  #'   as it can grab the initials for plots / legends (i.e. blue line is "DJ",
  #'   red is "NDJF"). This functions simply takes the initial for each element
  #'   in the vector, and then collapses it into a single string.
  #'
  #' @param x vector: A vector of strings; we want an initial from each string.
  #' @param collapse string: What should be between the initials? For example,
  #'   the default "" would give "DJF", whereas "_" would return "D_J_F" and "-"
  #'   returns "D-J-F".
  #'
  #' @examples
  #'      params <- list()
  #'      params$australSummer <- c("December", "January", "February")
  #'      params$borealSummer  <- c("June", "July", "August")
  #'      x1 <- get_initials(params$australSummer)
  #'      x2 <- get_initials(params$borealSummer, collapse = "-")
  #'
  #' @examples
  #'     params <- list()
  #'     params$austral <- c(12, 1, 2)
  #'     params$boreal  <- c(6, 7, 8)
  #'     x3 <- month.abb[params$austral] |> get_initials(collapse = "_")
  #'     x4 <- month.abb[params$boreal]  |> get_initials(collapse = ".")
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  y <- paste0(substring(x, 1, 1), collapse = collapse)
  return(y)
}

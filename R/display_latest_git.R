display_latest_git <- function(display = TRUE) {
  #' Print the information of the latest git commit
  #'
  #' @description When we use quarto notebooks, and render them to a pdf, we
  #'   don't necessarily know where the codebase was at for the pdf. In future,
  #'   we may want to go back to different version, but can only guess on the
  #'   date of rendering. This function is useful for such an instance because
  #'   it allows us to add the latest git commit before rendering so we have a
  #'   better idea. Requires the git2r package.
  #'
  #' @param display BINARY: Should the output be displayed? Usually, this is the
  #'   whole purpose of the function, but sometimes the output should be
  #'   suppressed, for example if adding the information to a list of data that
  #'   will then be saved in the Data/ folder.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  gitCommit <- (git2r::commits(getwd())[[1]])
  # Display / print git info
  if (isTRUE(display)) {
    cat("Latest git commit:")
    cat("\n     Commit : ", gitCommit$sha,
        "\n     Author : ", gitCommit$author[[1]],
        "\n     Date   : ", git2r::when(gitCommit),
        "\n     Message: ", gitCommit$message)
    print_line()
  }

  # Group git info to invisibly return
  info <- list("Commit:"  = gitCommit$sha,
               "Author:"  = gitCommit$author[[1]],
               "Date:"    = git2r::when(gitCommit),
               "Message:" = gitCommit$message,
               "git:"     = gitCommit)

  return(invisible(info))
}

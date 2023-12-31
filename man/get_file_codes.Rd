% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_file_codes.R
\name{get_file_codes}
\alias{get_file_codes}
\title{Return the file and project codes for the current file}
\usage{
get_file_codes(addGit = TRUE)
}
\arguments{
\item{addGit}{BINARY: Should the 7-digit SHA from the last git commit be
appended onto the file and project codes? Requires the \code{git2r} package.}
}
\description{
Sometimes when we print to a ".qmd" notebook, save some data,
or save a figure, it can be helpful to know exactly where it came from.
My system works on codes at the beginning of file and project names. The
idea of this function is therefore to quickly grab those codes, based on
the file where this function is called from. This function assumes that
the codes follow my own project and file name conventions.

The code conventions are:
project codes          xxx00_project_name
file codes             yy00_file_name.ext
file code w/ versions  yy00v0_file_name.ext

The function searches for any parts of the filename that contain a "\verb{_}",
splits it up around those "\verb{_}", and then assumes that the first ones are
interesting codes for us. We can optionally add the 7-digit SHA of the
last git commit to add some version control info if the
\href{https://docs.ropensci.org/git2r/}{git2r} package is installed. The
function only works in RStudio as far as I can tell, because it uses
\code{\link[rstudioapi:rstudio-editors]{rstudioapi::getSourceEditorContext()}} for retrieving the file name. It
is still a work-in-progress and needs some improvement!
}

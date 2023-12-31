% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_figure_directory.R
\name{create_figure_directory}
\alias{create_figure_directory}
\title{Create a new directory to store today's figures}
\usage{
create_figure_directory(relativePath = "", text = "inProgress")
}
\arguments{
\item{relativePath}{"string": Where is the "Figures/" directory (or where
should it be created if it doesn't already exist?). This directory is the
parent of the daily subdirectories.}

\item{text}{"string": What text should go in the directory name after the
current date? Defaults to "inProgress".}
}
\description{
When creating pdfs or png figures, it is easier to store them
in a separate directory each day. This function simply checks if a
directory already exists, creates a correctly named directory if not, and
returns the path so it can easily be written to.
}

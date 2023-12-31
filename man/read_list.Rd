% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_list.R
\name{read_list}
\alias{read_list}
\title{A very simple wrapper for reading a list with \code{jsonlite}}
\usage{
read_list(filename)
}
\arguments{
\item{filename}{The filename and path (relative to the working directory)
of the list.}
}
\description{
This function will be expanded to also accommodate yaml, rds
and rData output, and different json options. It currently just converts
a json file to an R list, using \code{\link[jsonlite:serializeJSON]{jsonlite::unserializeJSON()}}. It assumes
that it is reading a json file created by \code{\link[=save_list]{save_list()}}.
}

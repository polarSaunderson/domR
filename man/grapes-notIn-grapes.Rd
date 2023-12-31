% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notIn.R
\name{\%notIn\%}
\alias{\%notIn\%}
\title{The opposite of the native "in" function.}
\usage{
x \%notIn\% y
}
\arguments{
\item{x}{the values we are interested in}

\item{y}{the values to compare against}
}
\description{
If we want to know what isn't in something else, use this! It
returns a vector of binary values, not the actual values that are not in
there.
}
\examples{
  x <- c(1, 2, 3)
  y <- c(2, 3, 4, 5)
  z1 <- x \%notIn\% y    # should be TRUE,  FALSE, FALSE
  z2 <- y \%notIn\% x    # should be FALSE, FALSE, TRUE, TRUE

}

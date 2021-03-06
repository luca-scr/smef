% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe.R
\name{describe}
\alias{describe}
\alias{print.describe}
\title{Descriptive statistics}
\usage{
describe(data, by, detailed = FALSE, ...)

\method{print}{describe}(x, digits = getOption("digits"), ...)
}
\arguments{
\item{data}{a matrix or a data.frame.}

\item{by}{the name of a variable to condition on,}

\item{detailed}{logical, if TRUE a detailed summary is returned.}

\item{\dots}{additional arguments to be passed to the low level functions.}

\item{x}{an object of class `describe`.}

\item{digits}{significant digits.}
}
\description{
Descriptive statistics for a matrix or data frame.
}
\examples{

describe(iris, detailed = TRUE)
describe(iris, by = Species)

}

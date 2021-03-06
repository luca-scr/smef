% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confint.maxLik.R
\name{confint.maxLik}
\alias{confint.maxLik}
\title{Wald confidence intervals for Maximum Likelihood Estimates}
\usage{
\method{confint}{maxLik}(object, parm, level = 0.95, ...)
}
\arguments{
\item{object}{an object returned by `maxLik()` function.}

\item{parm}{the name of parameters to compute the confidence intervals.}

\item{level}{the level of confidence interval.}

\item{\dots}{additional arguments to be passed to the low level functions.}
}
\value{
A vector of lower and upper confidence interval limits.
}
\description{
Wald confidence intervals based on asymptotic Gaussian
distribution of MLE.
}
\examples{

# MLE of the rate parameter of exponential distribution
x <- rexp(100, 2.5)
loglik <- function(theta) sum(dexp(x, rate = theta, log = TRUE))
MLE <- maxLik(loglik, start = 1)
summary(MLE)
confint(MLE)

}

#' @name confint.maxLik
#' @aliases confint.maxLik
#' 
#' @title Wald confidence intervals for Maximum Likelihood Estimates
#' 
#' @description Wald confidence intervals based on asymptotic Gaussian
#' distribution of MLE.
#' 
#' @param object an object returned by `maxLik()` function. 
#' @param parm the name of parameters to compute the confidence intervals.
#' @param level the level of confidence interval.
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @return A vector of lower and upper confidence interval limits. 
#'
#' @examples
#' 
#' # MLE of the rate parameter of exponential distribution
#' x <- rexp(100, 2.5)
#' loglik <- function(theta) sum(dexp(x, rate = theta, log = TRUE))
#' MLE <- maxLik(loglik, start = 1)
#' summary(MLE)
#' confint(MLE)
#' 
#' @rdname confint.maxLik
#' @export

confint.maxLik <- function(object, parm, level = 0.95, ...)
{
  cf <- coef(object)
  if(missing(parm)) 
    parm <- seq_along(cf)
  pnames <- names(cf)
  if(is.null(pnames)) 
    pnames <- parm
  else if(is.numeric(parm)) 
         parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- format.perc(a, 3)
  q <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), 
              dimnames = list(parm, pct))
  se <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + se %o% q
  return(ci)
}

format.perc <- function(probs, digits) 
paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")

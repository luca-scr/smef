# Information criteria for glmnet models (Ridge, Lasse, ElasticNet)

ic.glmnet <- function(object, ...)
{
  stopifnot(inherits(object, "glmnet"))
	
  family <- as.list(object$call)$family
  if(is.null(family)) family <- "gaussian"
  stopifnot(any(family == c("gaussian", "binomial")))
  
  y <- eval(as.list(object$call)$y, envir = parent.frame())
  n <- object$nobs
  x <- eval(as.list(object$call)$x, envir = parent.frame())
  standardize = as.list(object$call)$standardize
  if(is.null(standardize)) standardize <- TRUE
  if(standardize)
  {
    x <- scale(x, center = FALSE, scale = sqrt(apply(x, 2, var)*(n-1)/n))
  }
  x <- cbind(1,x)

  alpha <- eval(as.list(object$call)$alpha, envir = parent.frame())
	if(is.null(alpha)) alpha <- 1
  lambda <- object$lambda
  nlambda <- length(lambda)
  beta <- as.matrix(coef(object))
  p <- nrow(beta)
  dev <- object$nulldev*(1-object$dev.ratio)
  loglik <- if(family == "gaussian") -0.5*n*log(dev/n) else -0.5*dev 

  # compute effective degrees of freedom
  # Zou, H., Hastie, T., & Tibshirani, R. (2007). On the "degrees of freedom" of the lasso. 
  #   Ann. Statist., 35(5), 2173–2192.
  df <- rep(as.double(NA), nlambda)
  active <- apply(abs(beta) > sqrt(.Machine$double.eps), 2, which)
  for(l in 1:nlambda)
  {
    # H <- x %*% solve(crossprod(x) + diag(lambda[l], p)) %*% t(x)
    # df[l] <- tr(H)
    # much faster
    if(length(active[[l]]) > 0)
    {
      evalues <- svd(x[,active[[l]],drop=FALSE], nu = 0, nv = 0)$d^2
      df[l] <- sum(evalues/(evalues+lambda[l]))
    } else
    { 
      df[l] <- 0
    }
  }
  aic  <- -2*loglik + 2 * df
  aicc <- -2*loglik + 2 * df * n/(n-df-1)
  bic  <- -2*loglik + log(n) * df
  
  out  <- list(family = family, alpha = alpha, lambda = lambda, 
		           dev = dev, dev.ratio = object$dev.ratio,
		           loglik = loglik, df = df, 
               aic = aic, aicc = aicc, bic = bic)
  return(out)
}


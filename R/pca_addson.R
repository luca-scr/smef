#
#  pca addson
#

prcompRecon <- function(object, npcs = 2, ...)
{
  stopifnot(inherits(object, "prcomp"))
	npcs <- min(as.integer(npcs),
	            sum(object$sdev > sqrt(.Machine$double.eps)))
  Z <- object$x[,seq(npcs),drop=FALSE]
  V <- object$rotation[,seq(npcs),drop=FALSE]
  x <- Z %*% t(V)
  if(is.numeric(object$scale))
    x <- scale(x, center = FALSE, scale = 1/object$scale)
  if(is.numeric(object$center))
    x <- scale(x, center = -object$center, scale = FALSE)
  return(x)
}

prcompReconError <- function(object, data, ...)
{
  stopifnot(inherits(object, "prcomp"))
  data <- as.matrix(data)
	npcs <- length(object$sdev)
	RE <- rep(as.double(NA), npcs)
	for(q in 1:npcs)
	{
    RE[q] <- sum((data - prcompRecon(object, npcs = q))^2)
	}
	MSE <- RE/prod(dim(data))
	Rsq <- 1 - RE/sum(scale(data, center = object$center, scale = FALSE)^2)

	out <- list(RE = RE, MSE = MSE, Rsq = Rsq)
  return(out)
}

tr <- function (x)
{
   x <- as.matrix(x)
   sum(diag(x))
}


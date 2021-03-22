#' @name plotImage
#' @aliases plotImage
#' 
#' @title Wald confidence intervals for Maximum Likelihood Estimates
#' 
#' @description Wald confidence intervals based on asymptotic Gaussian
#' distribution of MLE.
#' 
#' @param x = a matrix of data values for grey-scale images and a 3-dimensional
#' array for RGB images.
#' @param max = the maximum value of RGB color scale.
#' @param mar = the margin used in the base R graphic. 
#'
#' @rdname plotImage
#' @export

plotImage <- function(x, max = 255, mar = c(0,0,0,0), ...)
{
  x[x < 0] <- 0
  x[x > max] <- max
  x <- as.raster(x, max = max)
  h <- nrow(x)
  w <- ncol(x)
  par(mar = mar)
  plot(0, 0, type = "n", xlim = c(1, w), ylim = c(1, h),
       axes = FALSE, ann = FALSE, asp = 1)
  rasterImage(x, 1, 1, w, h, ...)
}

require(jpeg)

plotImage <- function(x, max = 255, mar = c(0,0,0,0), ...)
{
  x[x < 0] <- 0
  x[x > max] <- max
  x <- as.raster(x, max = max)
  w <- nrow(x)
  h <- ncol(x)
  par(mar = mar)
  plot(0, 0, type = "n", xlim = c(1, w), ylim = c(1, h),
       axes = FALSE, ann = FALSE, asp = 1)
  rasterImage(x, 1, 1, w, h, ...)
}

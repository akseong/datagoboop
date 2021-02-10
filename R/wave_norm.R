#' @title Normalize waves to ensure input to wplay() is valid
#'
#' @param y Either a numeric vector or a 2xn_samples matrix
#'
#' @return a normalized wave of the same dimension as the input
#' @export
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' wave_norm(sin(2 * pi * 60 * (1:1000) + 2 * pi * 100 * (1:1000)))
#'
wave_norm <- function(y) {

  # Cast as matrix, if not already
  y <- as.matrix(y)

  # Check which way the matrix is oriented
  trans <- FALSE
  if (which(dim(y) == min(dim(y))) == 1) {
    trans <- TRUE
    y <- t(y)
  }

  # ensures input to play() is valid
  # (apply affine transformation to y so that max(y) = 1 and min(y) = -1;
  #  anything above +1/below -1 is clipped by play(), resulting in distortion)

  # Remove DC offset
  bias <- colMeans(y, na.rm = TRUE)
  y <- y - bias
  M <- max(y, na.rm = TRUE)
  m <- min(y, na.rm = TRUE)

  # Find normalizing constant (scale == 0 implies y is all zeros)
  scaling <- if (abs(M) > abs(m)) abs(M) else abs(m)
  if (scaling > 0) {
    y <- y / scaling
  }

  # Replace NAs with zeros
  y[is.na(y)] <- 0

  # Orient the input as originally provided
  if (trans) {
    y <- t(y)
  }

  # Done.
  return(y)
}

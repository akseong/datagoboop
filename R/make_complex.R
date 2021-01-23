
#' @title Function to apply Hilbert transform within each band output by \code{decomp()}
#' @description Takes the real-valued output from \code{decomp()} and infers a complex-valued version of each row. This allows us to figure out the amplitude over time ("envelope") and phase of each component sine wave.
#' @param d output from \code{decomp()}
#' @param fs integer: sampling rate of the signal that was passed into \code{decomp()}
#'
#' @return list containing phase \code{phi} and amplitude \code{amp} of the complex waveform
#' @export
#' @importFrom seewave hilbert
#'
#' @examples
#' \dontrun{
#' # Create a signal that is a sum of harmonically related sinusoids
#' fs <- 44100
#' dur <- 2
#' t <- seq(0, dur, length.out = dur * fs)
#' f0 <- 200
#' n_harm <- 8
#' y <- sin(2 * pi * f0 * t)
#' for (k in 2:n_harm) {
#'   y <- y + sin(2 * pi * k * f0 * t)
#' }
#'
#' # Separate the sinusoids
#' yd <- decomp(y, n_harm = n_harm, fs = fs, f0 = f0)
#'
#' # Get the envelope and phase of each sinusoid
#' h <- make_complex(yd, fs = fs)
#'
#' # Show how the output of make_complex() is related to that of decomp()
#' from_decomp <- yd[1, 1:2000]
#' from_make_complex <- h$amp[1, 1:2000] * cos(h$phi[1, 1:2000])
#' plot(from_decomp, col = "red")
#' lines(from_make_complex, col = "blue")
#' }
make_complex <- function(d, fs = 44100) {

  # Initialize the matrix to contain the Hilbert-transformed decomposition
  H <- matrix(NA, nrow = nrow(d), ncol = ncol(d))

  for (r in 1:nrow(d)) {

    # hilbert() function in the library(seewave) applies the Hilbert transform to a wave
    # The Hilbert transform infers the complex waveform from a real waveform
    H[r, ] <- seewave::hilbert(d[r, ], fs)
  }

  # Arg of the complex sinusoid is phase     /  angle in the complex plane
  # Mod of the complex sinusoid is amplitude / radius in the complex plane
  return(list(
    "phi" = Arg(H),
    "amp" = Mod(H)
  ))
}

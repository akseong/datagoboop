# Decompose waveform y using a bank of harmonically related band-pass filters
#  Takes a sound (with pitch == f0), and passes it through a bank of band-pass filters;
#  each row of the output is a filtered version of the input

#' @title Decompose a waveform into sine waves
#' @description Decomposes the mono waveform \code{y} using a bank of harmonically related band-pass filters (i.e., they have center frequencies that are integer multiples of \code{f0}). Each row of the output is a filtered version of the input. If the input is a single complex tone with pitch \code{f0}, this function isolates the harmonics in the complex.
#'
#' @param y n_samples length numeric vector 
#' @param n_harm integer:  number of harmonics to analyze (i.e., the number of filters to use)
#' @param fs integer: The sampling rate of \code{y}
#' @param f0 numeric: The center frequency of the lowest band-pass filter
#'
#' @return Decomposition matrix. Each row represents one component sine wave in \code{y}, assuming \code{y} is composed of harmonically related sines. Summing the columns will closely approximate the original input \code{y}. 
#' @export
#' @importFrom seewave ffilter
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
#' # First row is the sinusoid at the fundamental frequency
#' plot(yd[1, 1:2000], type = "l")
#' }
decomp <- function(y, n_harm = 12, fs = 44100, f0 = 440) {
  
  # TODO: this can probably be made faster by pre-allocating matrix d
  d  <- c()
  
  for (k in 1:n_harm) {
    
    # ffilter() in library(seewave) can be used to implement a band-pass filter.
    # Use band-pass filters (BPFs) to break sound wave into its component frequencies.
    # Each BPF is centered on a harmonic frequency and spans the space between its two
    #  neighboring filters.
    fc     <- f0 * k
    band_l <- f0 * (k - 0.5)
    band_u <- f0 * (k + 0.5)
    
    # Append filtered output to the decomp matrix
    d <- rbind(d, t(seewave::ffilter(y, fs, from = band_l, to = band_u)))
  }
  
  # Done.
  return(d)
}

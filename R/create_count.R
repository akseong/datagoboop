# function to create 4 count of the specified dur
#' @title 4 count snare
#' @description function to create 4 count of the specified dur
#' @param dur numeric: duration of the output in seconds
#' @param fs integer: sampling rate of the output in Hertz
#'
#' @return numeric vector representing the waveform of the 4 count sound
#' @export
#' @importFrom stats rnorm
create_count <- function(dur, fs = 44100) {

  # Set up
  wave_length <- fs * dur
  wave_frac <- floor(wave_length / 4)
  wave_ret <- rep(0, wave_length)

  # iterating and creating a short snare sound at each beat
  start <- 1
  for (i in 1:4) {
    wave_ret[start:(start + 1000 - 1)] <- rnorm(1000)
    start <- start + wave_frac
  }
  return(wave_ret)
}

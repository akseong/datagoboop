#' @title Repitch your audio
#' @description Takes a single-channel (mono) waveform `y` and shifts it by `shift` semitones.
#' `shift` may be positive or negative, or non-integer. Note that shifting pitch is accomplished via time dilation so the new note will be shorter for positive values of shift and longer for negative values.
#' @param y numeric vector representing the waveform to be repitched
#' @param shift numeric: the number of semitones to pitch shift \code{y} by. A value of 0 will return the input \code{y} unchanged. Negative values will result in slower, lower pitched outputs; positive values will result in faster, higher pitched outputs. This argument need not be an integer.
#'
#' @return a numeric vector that represents the pitch-shifted version of \code{y}
#' @export
#' @importFrom stats approx
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' # Generate pure tone at 420 Hz
#' t <- seq(0, 1, length.out = 44100)
#' y <- sin(2 * pi * 420 * t)
#'
#' # Play it out loud
#' wplay(y)
#'
#' # Play it again after pitch-shifting down two semitones
#' wplay(repitch(y, -2))
#'
repitch <- function(y, shift) {
  # Takes a single-channel (mono) waveform `y`
  # and shifts it by `shift` semitones.
  # `shift` may be positive or negative, or non-integer.
  # Note that shifting pitch is accomplished via time dilation
  # so the new note will be shorter for positive values of shift
  # and longer for negative values.
  t_old <- seq(0, 1, length.out = length(y))
  t_new <- seq(0, 1, length.out = round(length(y) / 2^(shift / 12)))
  return(stats::approx(t_old, y, t_new)$y)
}

# Function to create continuous sliding sound.
#' @title Create continuous sliding sound.
#' @inheritParams sonify_box
#' @param hz int: The middle frequency of the sound
#' @param phase_size int: The range of values being traversed
#' @param dur int: The duration of the sound in seconds
#'
#' @return a n_samples length sound vector
#' @export
wurr <- function(hz = 440,
                 phase_size = 1,
                 phase_speed = 0.5,
                 dur = 6,
                 volume = 1,
                 fs = 44100) {
  t <- seq(from = 0, to = dur - 1 / fs, by = 1 / fs)
  phi <- phase_size * sin(2 * pi * phase_speed * t)
  wave <- volume * sin(2 * pi * (hz * t + phi))
  return(wave)
}

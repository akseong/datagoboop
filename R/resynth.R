
#' @title Synthesize amplitude-modulated complex tones
#' @description Synthesize tones using parameters to specify envelope, relative amplitude of harmonics, and relative phase of harmonics. Generally, these parameters will be derived from pre-recorded sounds, and this function will be called by \code{snd_inst()}.
#'
#' @param hz numeric: the frequency of the sound to be synthesized
#' @param dur numeric: the duration of the output sound in seconds
#' @param env numeric vector representing the envelope (amplitude over time) of the output sound at equidistant points spanning its entire duration.
#' @param amp numeric vector representing the amplitude of each harmonic in the output, starting from the fundamental frequency. If it is shorter than \code{phi}, then the missing entries will default to 1.
#' @param phi numeric vector representing the phase of each harmonic in the output, starting from the fundamental frequency. If it is shorter than \code{amp}, then the missing entries will default to 0.
#' @param fs_old integer: sampling frequency of the original sound used to infer \code{env}, \code{amp}, and \code{phi}. \strong{Now obsolete given the current synthesizer design}.
#' @param fs_new integer: sampling frequency of the output sound.
#'
#' @return n_samples length numeric vector representing the sound given by the above parameters.
#' @export
#' @importFrom stats approx
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' # Specify the parameters by hand for a 360 Hz complex tone with linear decay
#' w <- resynth(hz = 360, dur = 1, env = c(1, 0), 
#'              amp = rev(seq(0, 1, length.out = 6)), 
#'              phi = rep(0, 6))
#'
#' # Play it out loud
#' wplay(w)
#'
resynth <- function(hz, dur, env, amp, phi, fs_old = 44100, fs_new = 44100) {

  # Set up time vectors to help with interpolation and stretching
  len <- floor(dur * fs_new)
  t_old <- seq(0, length(env) / fs_old, length.out = length(env))
  t_int <- seq(0, length(env) / fs_old, length.out = len)
  t_new <- seq(0, dur, length.out = len)

  # Generate the envelope
  env_new <- stats::approx(t_old, env, t_int)$y

  # Generate the waveform
  y <- matrix(0, nrow = len, ncol = 1)
  for (k in 1:length(amp)) {
    p <- if (k > length(phi)) 0 else phi[k]
    a <- if (k > length(amp)) 1 else amp[k]
    y <- y + env_new * a * sin(2 * pi * hz * k * t_new + p)
  }

  # Done.
  return(wave_norm(y))
}

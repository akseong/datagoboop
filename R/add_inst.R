# All the instrument data, saved as a global variable
inst <- readRDS(here::here("sounds", "instruments.rds"))

#' @title Add an instrument
#' @description Takes a user-specified sound (stored at \code{filepath} on the local machine), infers synthesizer parameters from the sound, and saves the synthesizer parameters to be used later (e.g., via \code{notes()}) under the label \code{inst_lab}.
#'
#' @param filepath string: the location of a brief sound file that the synthesizer will try to emulate.
#' @param f0 numeric: the fundamental frequency of the input sound in Hertz. This defaults to 440 (A above middle C in concert tuning), so the user should either provide a sound file that matches this pitch or determine the appropriate \code{f0} of the sound file they are using.
#' @param fs integer: the sampling frequency (or frame rate) of the sound file in Hertz. This defaults to 44100, but 48000 is not uncommon. The user should check the frame rate of their sound file beforehand.
#' @param inst_lab string: the label that will be used to refer to the synthesizer parameters generated. Note that "bass", "piano", "string", and "wind" are already assigned by default and cannot be overwritten. This string is used to index a \code{list}, so verify that \code{inst_lab} is a valid list index. 
#'
#' @return -1 if the user tries to assign an \code{inst_lab} that already exists. Otherwise, returns nothing.
#' @export
#'@importFrom audio load.wave
#'@importFrom here here
#'@importFrom utils head
#'
#' @examples
#' \dontrun{
#' # Generate a 250 Hz sawtooth wave and save it in the local folder using the save.wave() function from library(audio)
#' fs <- 44100
#' f0 <- 250
#' dur <- 2
#' t <- seq(0, dur, length.out = dur * fs)
#' sawtooth <- 2 * ((250 * t) %% 1) - 1
#' audio::save.wave(what = sawtooth, where = here::here("example.wav"))
#' 
#' # Open the newly saved sound file and use it to specify a new synthesizer sound under the label "saw".
#' add_inst(here::here("example.wav"), f0 = f0, fs = fs, inst_lab = "saw")
#' 
#' # Use the newly generated sound in a short melody
#' melody <- c(40, 38, 36, 38, 40, 40, 40)
#' wplay(notes(melody, dur = 0.25, vol = 0.3, inst_lab = "saw"), normalize = FALSE)
#' }
add_inst <- function (filepath, f0 = 440, fs = 44100, inst_lab) {
  
  # Check that this won't overwrite an existing sound
  if (inst_lab %in% names(inst)) {
    warning(paste("The instrument name `", inst_lab, "` already exists. Quitting."))
    return(-1)
  }
  
  #Load the sound
  w <- audio::load.wave(filepath)
  
  # Make mono for simplicity
  if (ncol(as.matrix(w)) > 1) {
    w <- w[, 1]
  }
  
  # Decompose the sound into its component sine waves
  d <- decomp(w, n_harm = 50, f0 = f0, fs = fs)
  
  # Get the envelope (amp) and phase (phi) of each sine wave
  H <- make_complex(d, fs = fs)
  
  # Get the mean envelope
  env <- colMeans(H$amp)
  peak <- which(env == max(env))
  
  # Simplify it to just 100 time points
  env <- env[seq(1, length(env), length.out = 100)]
  
  # Remove the quiet tail
  exceed = FALSE
  cut <- 0
  while (!exceed) {
    cut <- cut + 1
    if (rev(env)[cut] >= max(env) / 12) {
      exceed <- TRUE
    }
  }
  env <- utils::head(env, -cut)
  
  # Then quickly ramp down from the cut point
  damp <- cos(seq(0, pi, length.out = 8)) / 2 + 0.5
  env <- env * c(rep(1, length(env) - length(damp)), damp)
  
  # Get the relative amplitudes of the harmonics
  amp_f <- colMeans(t(as.matrix(H$amp)))
  
  # Get the relative phase of the harmonics
  phi_f = H$phi[, peak[1]]
  
  # Save the results
  inst[[inst_lab]] <<- list("env" = env, "amp_f" = amp_f, "phi_f" = phi_f)
  
  # Save the results for later
  saveRDS(inst, here::here("sounds", "instruments.rds"))
}


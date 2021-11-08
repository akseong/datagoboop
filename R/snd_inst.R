
#' @title Synthesize a sound by specifying the instrument name
#' @description A wrapper for \code{resynth()} that allows the user to specify the instrument that they want to play the note using its label \code{inst_lab}. The user may add new sounds to the base library using \code{add_inst()}.
#'
#' @param hz numeric: frequency of interest for playback
#' @param dur integer: duration of the sound in seconds
#' @param inst_lab string: instrument to be used. Valid input: "piano", "string", "wind", "bass", or label for sound created using \code{add_inst()}
#' @param dir_custom string: directory where instruments_custom.rda is saved. Not necessary if using only present sounds. If unspecified, defaults to the working directory.
#' 
#' @return numeric vector representing the sound to be played
#' @export
#' @importFrom here here
#' @importFrom utils data
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' # Generate a two-second sound of synthesized flute at 320 Hz
#' w <- snd_inst(hz = 320, dur = 2, inst_lab = "wind")
#'
#' # Play it out loud
#' wplay(w)
#'
snd_inst <- function(hz, dur, inst_lab = "piano", dir_custom) {
  
  # Use current working directory if `dir_custom` not specified
  if (missing(dir_custom)) {
    dir_custom <- here::here()
  }
  
  # Load the instrument data included in DataGoBoop
  data("inst")
  
  # Check that the requested inst_lab exists either in presets
  if (inst_lab %in% names(inst)) {
    # If desired instrument exists as preset, use the preset as source
    inst_src <- inst
  }
  else {
    # Otherwise, desired instrument does not exist as preset, check for matching custom instrument
    custom_file <- paste(dir_custom, "/inst_custom.rda", sep = "")
    if (file.exists(custom_file)) {
      load(custom_file)
      if (inst_lab %in% names(inst_custom)) {
        # If desired instrument exists in inst_custom, use custom instrument as source
        inst_src <- inst_custom
      }
      else {
        # Otherwise, the desired instrument is neither a preset or custom
        stop(paste("The instrument name `", inst_lab, "` is not a preset instrument and does not exist in ", custom_file, ". Quitting."))
      }
    }
    else {
      stop(paste("The instrument name `", inst_lab, "` is not a preset instrument and no instruments_custom.rda exists at ", dir_custom, ". Quitting."))
    }
  }
  
  # If no error was thrown above, the desired instrument exists 
  resynth(hz, dur,
          env = inst_src[[inst_lab]]$env,
          amp = inst_src[[inst_lab]]$amp_f,
          phi = inst_src[[inst_lab]]$phi_f)
}

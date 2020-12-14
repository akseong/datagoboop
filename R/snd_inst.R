
#' @title Synthesize a sound by specifying the instrument name
#' @description A wrapper for \code{resynth()} that allows the user to specify the instrument that they want to play the note using its label \code{inst_lab}. The user may add new sounds to the base library using \code{add_inst()}. 
#'
#' @param hz numeric: frequency of interest for playback 
#' @param dur integer: duration of the sound in seconds
#' @param inst_lab string: instrument to be used. Valid input: "piano", "string", "wind", "bass", or label for sound created using \code{add_inst()}
#'
#' @return numeric vector representing the sound to be played
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a two-second sound of synthesized flute at 320 Hz
#' w <- snd_inst(hz = 320, dur = 2, inst_lab = "wind")
#' 
#' # Play it out loud
#' wplay(w)
#' }
snd_inst <- function(hz, dur, inst_lab = "piano") {
  resynth(hz, dur, env = inst[[inst_lab]]$env, amp = inst[[inst_lab]]$amp_f, phi = inst[[inst_lab]]$phi_f)
}


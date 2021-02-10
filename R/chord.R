#' Make chords
#'
#' Input piano keys in desired chord; outputs playable wave for a chord
#'
#' @param pkey numeric vector: pitch expressed in piano key numbers (ideal piano).  40 corresponds to middle C / C4.
#' @inheritParams note
#'
#' @return dur*fs length numeric vector of the chord.
#' @export
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' wplay(chord(pkey = c(40, 44, 47), dur = 3))
#'
chord <- function(pkey, vol = 1, dur = 1, fs = 44100, inst_lab = "sine", decay = FALSE, decay_rate = 6) {
  # input: piano keys in desired chord;
  # output: playable wave for a chord
  # add waves for each note together, then normalize
  # vol and dur can be vectors of same length as pkey.
  if (inst_lab != "sine" & decay == T) {
    warning("synthesized instruments (inst_lab) already have built-in decay.")
  }

  # check lengths
  err_vol <- (length(vol) != length(pkey) & length(vol) != 1)
  err_dur <- (length(dur) != length(pkey) & length(dur) != 1)
  if (err_vol | err_dur) {
    stop("vol and dur must either be length 1 or the same length as pkey")
  }

  if (length(vol) == 1) vol <- rep(vol, length(pkey))
  if (length(dur) == 1) dur <- rep(dur, length(pkey))

  waves <- note(
    pkey = pkey[1],
    vol = vol[1],
    dur = dur[1],
    fs = fs,
    inst_lab = inst_lab,
    decay = decay,
    decay_rate = decay_rate
  )
  if (length(pkey) > 1) {
    for (i in 2:length(pkey)) {
      waves <- waves + note(
        pkey = pkey[i],
        vol = vol[i],
        dur = dur[i],
        fs = fs,
        inst_lab = inst_lab,
        decay = decay,
        decay_rate = decay_rate
      )
    }
  }
  wave_norm(waves)
}

#' @title Note: play a pitch
#'
#' Turns piano key number into playable wave.  (use \code{note()} for a single piano key; use \code{notes()} for a sequence)
#' @param pkey numeric: pitch expressed in piano key numbers (ideal piano).  40 corresponds to middle C / C4.
#' @param vol numeric: relative volume of the sound, 1 is the maximum for a clear sound.
#' @param dur integer: duration of the sound in seconds.
#' @param fs integer: sampling rate (default: \code{44100}.  Sampling rate is dependent on user audio system, typically 44100 or 48000).
#' @param inst_lab string: instrument used.  Valid input: \code{"sine"} (default), \code{"piano"}, \code{"string"}, \code{"wind"}, \code{"bass"}, or label for sound created using \code{add_inst()}
#' @param decay logical: if \code{TRUE}, volume of individual notes decays over duration of note (default: \code{FALSE})
#' @param decay_rate numeric: The decay rate of the sound. Larger -> Faster decay. (negative numbers result in sound growing louder over duration of note)
#' @param progbar logical: display progress bar (default: \code{TRUE})
#'
#' @return dur*fs length numeric vector
#' 
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' wplay(note(40))
#' wplay(notes(40, 44, 47))
#'
note <- function(pkey, vol = 1, dur = 1, fs = 44100, inst_lab = "sine", decay = FALSE, decay_rate = 6) {
  # turns piano key into playable wave (non-vectorized)
  # dur = duration.  dur = 1 corresponds to 1 second of playback
  # inst_lab: can specify use of synthesized instruments
  # decay, decay_rate: if TRUE, volume decays exponentially
  #      (decay = TRUE, decay_rate = 6 emulates a plucked string)

  # NA handling
  if (is.na(pkey)) {
    return(rep(0, floor(dur * fs)))
  }

  # cap volume to prevent clipping
  V <- min(abs(c(vol, 1)))

  # combines basic funcs to output playable vectors
  if (inst_lab == "sine") {
    wave <- V * hz_to_sec(hz = pkey_to_hz(pkey = pkey), dur, fs)
  }
  else {
    wave <- V * snd_inst(hz = pkey_to_hz(pkey = pkey), dur, inst_lab)
  }

  # enforce wave vector length
  length_wave <- floor(dur * fs)
  wave <- wave[1:length_wave]

  if (!decay) {
    return(wave)
  } else if (is.numeric(decay_rate)) {
    decay_amp <- exp(-decay_rate * (1:length(wave) - 1) / fs)
    return(wave * decay_amp)
  } else {
    stop("if decay = TRUE, decay rate must be numeric")
  }
}


#' @describeIn note vectorized version of note()
#' @export
notes <- function(pkey, vol = 1, dur = 1, fs = 44100, inst_lab = "sine", decay = FALSE, decay_rate = 6, progbar = TRUE) {
  # vectorized version of note() (relies on note())
  # allow pkey, vol, dur, inst_lab to be vectors as well.
  n_keys <- length(pkey)

  # check lengths
  err_vol <- (length(vol) != n_keys & length(vol) != 1)
  err_dur <- (length(dur) != n_keys & length(dur) != 1)
  err_inst <- (length(inst_lab) != n_keys & length(inst_lab) != 1)
  err_dec <- (length(decay) != n_keys & length(decay) != 1)
  err_rate <- (length(decay_rate) != n_keys & length(decay_rate) != 1)
  if (err_vol | err_dur | err_inst | err_dec | err_rate) {
    stop("vol, dur, inst_lab, decay, and decay_rate must either be length 1 or the same length as pkey")
  }

  if (length(vol) == 1) vol <- rep(vol, n_keys)
  if (length(dur) == 1) dur <- rep(dur, n_keys)
  if (length(inst_lab) == 1) inst_lab <- rep(inst_lab, n_keys)
  if (length(decay) == 1) decay <- rep(decay, n_keys)
  if (length(decay_rate) == 1) decay_rate <- rep(decay_rate, n_keys)


  wave_lengths <- floor(dur * fs)
  wave_ret <- numeric(sum(wave_lengths)) # pre-allocate for speed
  st_end <- c(0, cumsum(wave_lengths)) # track vector start/end
  pb_needed <- progbar & any(inst_lab != "sine") # progress bar only needed if using synthesized instruments

  # initialize progress bar
  if (pb_needed) {
    pb <- txtProgressBar(min = 0, max = n_keys, style = 3)
  }
  for (i in 1:n_keys) {
    wave_ret[(st_end[i] + 1):(st_end[i + 1])] <- note(
      pkey = pkey[i],
      vol = vol[i],
      dur = dur[i],
      fs = fs,
      inst_lab = inst_lab[i],
      decay = decay[i],
      decay_rate = decay_rate[i]
    )
    # add tick to progressbar
    if (pb_needed) setTxtProgressBar(pb, i)
  }
  if (pb_needed) close(pb)
  return(wave_ret)
}

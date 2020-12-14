#' @title piano key number to frequency (hz) to soundwave
#'
#'@description
#' \code{pkey_to_hz()} and \code{hz_to_sec()} are usually used in combination.  
#' + \code{pkey_to_hz()} maps piano key number to hz (i.e. input = pitch, output = frequency) using "idealized standard piano" frequencies. 
#' + \code{hz_to_sec()} generates a playable soundwave vector at the specified frequency and duration.
#' 
#' @md
#' 
#' @inheritParams note
#' @param hz numeric: frequency of soundwave
#'
#' 
#' @return  
#' + \code{pkey_to_hz()}: numeric frequency .
#' + \code{hz_to_sec()}: numeric vector (playable with \code{audio::play()}, \code{cplay()}, or \code{wplay()}) 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # 49 is the piano key number for A440 (i.e. should return 440hz).  
#' pkey_to_hz(49)
#' 
#' # vector for A440
#' A440 <- hz_to_sec(440)
#' wplay(A440)
#' 
#' # play middle C / C4
#' wplay(hz_to_sec(pkey_to_hz(40)))
#' }
pkey_to_hz <- function(pkey){
  # NA handling; hz_to_sec returns vector of 0's if input hz = 0
  if (is.na(pkey)) return(0)
  2^((pkey-49) / 12) * 440
}

#' @describeIn pkey_to_hz generates playable vector with specified frequency (\code{hz})
#' @export
hz_to_sec <- function(hz, dur = 1, fs = 44100){
  # generates vector at specified frequency and duration
  # Note that fs is the "sampling frequency" i.e., 
  # the number of elements in the vector played over 1 second.
  # For most systems, this is either 44100 Hz or 48000 Hz
  len <- floor(dur * fs)
  t <- seq(0, dur, length.out = len)
  # Return the sine wave
  sin(2 * pi * hz * t)
}
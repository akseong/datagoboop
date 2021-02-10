#' Make a scale
#'
#' Generates piano key designations for (ascending) scale at specified tonic and number of octaves
#' @param tonic_pkey numeric: the root pitch from which to begin the scale (if \code{tonic_pkey = 0} generates semitones for major/minor scale of specified octaves)
#' @param n_octaves integer: the number of octaves ascending from \code{tonic_pkey}
#' @param scale_type string: "major" (default) or "minor"; specify the type of scale to create
#'
#' @return an integer vector of piano key numbers corresponding to the created scale
#' @export
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' x <- major_scale(49)
#' wplay(notes(x, dur = 1 / 8))
#'
make_scale <- function(tonic_pkey, n_octaves = 1, scale_type = "major") {
  # creates a vector of piano key numbers corresponding
  # to notes of a major/minor scale starting at pkey.
  # e.g. pkey=40 gives the piano keys for a scale beginning at middle C (C4)

  if (scale_type == "major") {
    intervals <- major_semitones()
  } else if (scale_type == "minor") {
    intervals <- minor_semitones()
  } else {
    stop("scale_type must be 'major' or 'minor'")
  }

  # construct scale one octave at a time
  pkeys <- rep(NA, 7 * n_octaves + 1)
  for (i in 1:n_octaves) {
    oct_st <- (i - 1) * 7 + 1
    oct_end <- i * 7 + 1
    pkeys[oct_st:oct_end] <- tonic_pkey + (i - 1) * 12 + intervals
  }

  pkeys
}



#' @describeIn make_scale generates semitones from tonic for each note in 1 octave of an ascending major scale
#' @export
major_semitones <- function() c(0, 2, 4, 5, 7, 9, 11, 12)

#' @describeIn make_scale generates semitones from tonic for each note in 1 octave of an ascending minor scale
#' @export
minor_semitones <- function() c(0, 2, 3, 5, 7, 8, 10, 12)



#' @describeIn make_scale generates an ascending major scale from the tonic
#' @export
major_scale <- function(tonic_pkey, n_octaves = 1) {
  make_scale(tonic_pkey = tonic_pkey, n_octaves = n_octaves, scale_type = "major")
}

#' @describeIn make_scale generates an ascending minor scale from the tonic
#' @export
minor_scale <- function(tonic_pkey, n_octaves = 1) {
  make_scale(tonic_pkey = tonic_pkey, n_octaves = n_octaves, scale_type = "minor")
}

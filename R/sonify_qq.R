

#' @title A sonified version of a QQ plot.
#' @description short summary
#' @inheritParams sonify_studres
#' @param progbar logical: if \code{TRUE} progress bar enabled
#'
#' @return A 2 x n matrix representing the sound to be played through each ear. Note that each tone is an observation/quantile, and tones that sound panned to the left are above the \code{y = x} line, and tones that sound panned to the right are below the \code{y = x} line.
#' @export
#' @importFrom stats rstandard qnorm
#' @importFrom graphics abline
#' @importFrom utils tail
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' # Run a linear regression on the mtcars data set
#' mod <- lm(mpg ~ 1 + cyl + hp + wt, data = mtcars)
#'
#' # Play the QQ plot for this model fit
#' wplay(sonify_qq(mod))
#'
sonify_qq <- function(lm_obj, tonic_pkey = 34, show_plot = FALSE, progbar = TRUE) {
  #  Sounds that appear in the left ear are on the
  # positive side of the x=y line, sounds that appear in the right ear on the negative
  # side of the x=y line. Sounds that appear in the center are near to the x=y line.
  # Imagine walking down the x=y line in the positive direction, away from the origin

  # Get standardized residuals
  ze <- sort(rstandard(lm_obj))
  n <- length(ze)

  # Get theoretical quantiles
  qt <- qnorm((1:n - 0.5) / n)

  # Show QQ plot if requested
  if (show_plot) {
    plot(x = qt, y = ze)
    abline(a = 0, b = 1)
  }

  # Bin the deviations from normality
  # (add a zero to the end of the deviations vector, used to set tonic pitch)
  dev <- c(ze - qt, 0)
  binned <- scale_num(dev, new_scale = 1:13)

  # The number of bins to put map to pitches below the tonic
  ii <- binned[length(binned)]
  n_below <- ii - 1

  # Remove the tracker at the end
  binned <- binned[1:(length(binned) - 1)]

  # Convert the bins to semitones
  below <- c(major_semitones()[1:7] - 24, major_semitones()[1:7] - 12)
  above <- c(major_semitones()[1:7], major_semitones()[1:7] + 12)
  bin_map <- c(tail(below, n_below), head(above, 13 - n_below))
  qq_notes <- bin_map[binned] + tonic_pkey



  #### Pitch-based version (less helpful to my ear)
  # # Generate a waveform for the sequence
  # w_qq <- notes(qq_notes, dur = 0.25, inst_lab = "piano")
  #
  # # Generate a tonic sequence to provide a reference for the x=y line
  # w_i <- note(tonic_pkey, dur = 0.25, inst_lab = "piano")
  # w_xy <- rep(c(w_i, rep(0, length(w_i))), ceiling(length(qq_notes) / 2))
  #
  # # Generate a bassline sequence that alternates between high and low tonic,
  # # indicating above/below x=y
  # qq_bass <- (binned < ii) * -12 + (binned > ii) * 12 + tonic_pkey
  # w_qq_bass <- notes(qq_bass, dur = 0.25, inst_lab = "bass")
  #
  # # Play the resultant composition
  # wplay(t(cbind(w_xy + w_qq_bass, w_xy + w_qq)))



  ### Stereo-based version
  qq_notes_l <- qq_notes
  qq_notes_r <- qq_notes
  qq_notes_l[binned < ii] <- NA
  qq_notes_r[binned > ii] <- NA
  w_qq_l <- notes(qq_notes_l, dur = 0.25, inst_lab = "piano", progbar = progbar)
  w_qq_r <- notes(qq_notes_r, dur = 0.25, inst_lab = "piano", progbar = progbar)

  # Generate a tonic sequence to provide a reference for the x=y line
  w_i <- note(tonic_pkey - 12, vol = 0.5, dur = 0.25, inst_lab = "piano")
  w_xy <- rep(c(w_i, rep(0, length(w_i))), ceiling(length(qq_notes) / 2))

  # Put it all together (using master tracks to make sure there are no dimension mismatches)
  track_l <- rep(0, max(length(w_qq_l), length(w_qq_r), length(w_xy)))
  track_r <- rep(0, max(length(w_qq_l), length(w_qq_r), length(w_xy)))
  track_l[1:length(w_qq_l)] <- track_l[1:length(w_qq_l)] + w_qq_l
  track_r[1:length(w_qq_r)] <- track_r[1:length(w_qq_r)] + w_qq_r
  track_l[1:length(w_xy)] <- track_l[1:length(w_xy)] + w_xy
  track_r[1:length(w_xy)] <- track_r[1:length(w_xy)] + w_xy

  # Done (apply wave_norm in case user wants to play right away).
  return(wave_norm(t(cbind(track_l, track_r))))
}

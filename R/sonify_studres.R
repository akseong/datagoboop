
#' @title Sonification of externally studentized residuals.
#' @description short summary
#' @param lm_obj lm object returned by base R's \code{lm} function
#' @param tonic_pkey integer: the piano key of the tonic note used in the sonification. Both sonifications rely on a major scale.
#' @param alpha numeric: the false alarm rate (i.e., the p-value) to be used to detect observations that deviate from normality. Note that this will be a two-sided test, and \code{alpha} represents the cumulative false alarm rate.
#' @param show_plot boolean: whether or not a rudimentary plot of the sonification should also be shown. Default: FALSE.
#'
#' @return A 1 x n matrix representing the sound to be played. Each chord is an observation. Observations that are below the alpha threshold will sound harmonious and will move up and down in pitch based on their relation to the mean. Observations that exceed the threshold will sound cacophonous.
#' @export
#' @importFrom MASS studres
#' @importFrom graphics abline
#' @importFrom stats qt
#'
#' @examples
#' \dontrun{
#' # Run a linear regression on the mtcars data set
#' mod <- lm(mpg ~ 1 + cyl + hp + wt, data = mtcars)
#'
#' # Play the studentized residuals for this model fit
#' wplay(sonify_studres(mod))
#' }
sonify_studres <- function(lm_obj, tonic_pkey = 34, alpha = 0.05, show_plot = FALSE) {
  #  If the residual
  # differs significantly from the null, a dissonant chord is played.
  # Otherwise, each residual is represented as a major chord.

  # Externally studentized (jackknife) residuals
  ee <- MASS::studres(lm_obj)

  # Plot studentized residuals if requested
  if (show_plot) {
    plot(ee)
    abline(a = 0, b = 0)
  }

  # They should follow a t(n - p - 1) distribution under normal error. Find crit values
  lo <- qt(alpha / 2, df = length(lm_obj$fitted.values) - length(lm_obj$coefficients) - 1)
  hi <- qt(1 - alpha / 2, df = length(lm_obj$fitted.values) - length(lm_obj$coefficients) - 1)

  # Convert residuals to notes
  ee_notes <- scale_num(ee, tonic_pkey, n_octaves = 1)

  # Put the notes into a waveform
  w <- c()
  for (k in 1:length(ee_notes)) {
    # If exceeds crit on positive or negative side, play dissonant chord
    if (ee[k] > hi || ee[k] < lo) {
      w <- c(w, 0.7 * chord(c(ee_notes[k], ee_notes[k] + 0.5, ee_notes[k] + 1), dur = 0.25, inst_lab = "piano"))
    }

    # Otherwise just play a nice chord with overlaid melody
    else {
      w <- c(w, chord(c(tonic_pkey - 12, tonic_pkey - 5, tonic_pkey), dur = 0.25, inst_lab = "wind")
      + note(ee_notes[k], dur = 0.25, inst_lab = "piano"))
    }
  }

  # Done.
  return(w)
}

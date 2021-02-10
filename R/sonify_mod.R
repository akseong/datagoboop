#' @title Wrapper function for sonify_qq() and sonify_studres()
#' @description Used to call \code{sonify_qq()} and \code{sonify_studres()} via a single function. Both of these subsidiary functions take an `lm` object as their input and produce a sonfied version of a standard model diagnostic.
#' @inheritParams sonify_studres
#' @param mode string: specifies which sonification mode to use. Options: {"qq", "studres"}. "qq" produces an auditory QQ plot, and "studres" sonifies the externally studentized residuals.

#'
#' @return A 1 x n (if \code{mode == "studres"}) or 2 x n (if \code{mode == "qq"}) matrix representing the sound to be played. Generally, the user should pass this directly to \code{wplay()}.
#' @export
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' # Run a linear regression on the mtcars data set
#' mod <- lm(mpg ~ 1 + cyl + hp + wt, data = mtcars)
#'
#' # Play the QQ plot for this model fit
#' wplay(sonify_mod(mod, mode = "qq"))
#'
#' # Play the studentized residuals for this model
#' wplay(sonify_mod(mod, mode = "studres"))
#'
sonify_mod <- function(lm_obj, mode = "qq", tonic_pkey = 34, alpha = 0.05, show_plot = FALSE) {
  # Wrapper function for sonify_qq() and sonify_studres()
  if (mode == "qq") {
    sonify_qq(lm_obj, tonic_pkey = tonic_pkey, show_plot = show_plot)
  }
  else if (mode == "studres") {
    sonify_studres(lm_obj, tonic_pkey = tonic_pkey, alpha = alpha, show_plot = show_plot)
  }
}

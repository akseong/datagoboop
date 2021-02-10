
#' @title Map your data to a scale
#' @description Place \code{x} values in equally-spaced bins (i.e. like a histogram's bins), then maps bins to specified scale (default scale is C4 major, 2 octaves).  Number of bins = number of notes in specified scale (i.e. dependent on \code{n_octaves}).
#'
#' @param x numeric vector: variable to be binned and mapped to scale
#' @inheritParams make_scale
#' @param new_scale numeric vector: vector of piano key number designations (can be fractional) to map bins to (if specified, overrides \code{tonic_pkey} and \code{scaletype} arguments).
#' @param by_quantile logical: if \code{TRUE}, bins values using equally-spaced quantiles as bin breakpoints (i.e. roughly equal number of \code{x}-values fall in each bin)
#' @param under_hood logical: if \code{TRUE}, returns list of most objects used to construct the mapping.
#'
#' @return either a numerical vector of piano key numbers (if \code{under_hood = FALSE}; default), or a list of objects used to construct the mapping (if \code{under_hood = TRUE}).
#' @export
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' scale_num(mtcars$mpg)
#'
#' # looking under the hood:
#' x <- scale_num(mtcars$mpg, under_hood = TRUE)
#' x
#' wplay(notes(x$df$x_pitch, dur = 1 / 4, vol = 0.25), normalize = FALSE)
#'
#' # specifying a different scale: e.g. equally-spaced arpeggio
#' new_scale <- c(16, 20, 23, 25) + rep(0:4 * 12, each = 4)
#' y <- scale_num(mtcars$mpg, new_scale = new_scale)
#' wplay(notes(y, dur = 1 / 4, vol = .25), normalize = FALSE)
#'
scale_num <- function(x,
                      tonic_pkey = 40,
                      n_octaves = 2,
                      scale_type = "major",
                      new_scale = NULL,
                      by_quantile = FALSE,
                      under_hood = FALSE) {
  # bins values in x, maps bins to notes in specified scale.
  # number of bins specified via # notes in scale
  # INPUT:
  #   x = numeric  variable.  higher values --> higher pitches in scale
  #   tonic_pkey:  first note in scale (default value 40 is middle C / C4)
  #   scale_type:  specify "major" or "minor"
  #   n_octaves:   number of octaves.
  #   new_scale:   vector of user-specified pitches to use.
  #                lowest-valued bin of x mapped to first element of vector,
  #                highest-valued bin mapped to last element
  #   by_quantile: can bin x by quantile or evenly-spaced bins.  Default is even spacing.
  #   under_hood:  returns everything used to

  if (!is.numeric(x)) stop("x must be a numeric vector")

  if (!is.null(new_scale)) {
    pitches <- new_scale
  } else {
    pitches <- make_scale(tonic_pkey, n_octaves, scale_type)
  }

  n_pitches <- length(pitches)

  if (by_quantile) {
    breaks <- stats::quantile(x, probs = seq(0, 1, length.out = n_pitches), na.rm = TRUE)
  } else {
    breaks <- seq(min(x, na.rm = TRUE) - .001 * abs(diff(range(x, na.rm = TRUE))),
      max(x, na.rm = TRUE) + .001 * abs(diff(range(x, na.rm = TRUE))),
      length.out = n_pitches
    )
  }

  x_cut <- cut(x, breaks = breaks, include.lowest = TRUE)
  x_pitch <- pitches[as.numeric(x_cut)]

  # output: option to "get everything"
  if (under_hood) {
    breaks_mat <- utils::read.table(text = gsub("[^.0-9]", " ", levels(x_cut)), col.names = c("lower", "upper"))
    res <- list(
      "df_x" = data.frame(x, x_cut, x_pitch),
      "breaks" = breaks,
      "breaks_mat" = breaks_mat,
      "n_pitches" = n_pitches,
      "pitches" = pitches,
      "tonic_pkey" = tonic_pkey,
      "n_octaves" = n_octaves,
      "scale_type" = scale_type
    )
    return(res)
  } else {
    return(x_pitch)
  }
}

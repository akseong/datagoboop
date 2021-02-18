

#' @title Sonified Histogram
#' @description Map the relative frequencies to the pitches of the songs that are played in ascending order of your variable.
#' @param data a dataframe
#' @param var the variable to create the sonified histogram for.
#' @param breaks int: the number of breaks. Sounds best if this entry is 2^n for some n.
#' @param tonic int: the pkey with which the root the bottom note is mapped to.
#' @param inst string: the name of the instrument used for playback
#' @param duration int: the length of playback in seconds
#' @param volume numeric between 0 and 1: the relative volume of playback
#' @param fs int: The sampling rate for playback
#' @param progbar logical: if \code{TRUE} progress bar enabled
#'
#' @return numeric vector representing the waveform of the sonified histogram
#' @export
#' @importFrom graphics hist
#' @importFrom stats IQR
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' dat <- data.frame(x = rnorm(100))
#' wplay(sonify_hist(dat, x))
#'
sonify_hist <- function(data,
                        var,
                        breaks = "Sturges",
                        tonic = 40,
                        inst = "sine",
                        duration = 2,
                        volume = 0.75,
                        fs = 44100,
                        progbar=TRUE) {

  # Reading in data
  x <- data %>%
    select({{ var }}) %>%
    as_vector()


  # Creating the number of breaks for the histogram. Current standard is a multiple of 4.
  if (breaks == "Sturges") {
    n_breaks <- ceiling(log2(length(x)) + 1)
    n_breaks <- n_breaks + (4 - n_breaks %% 4) + 1
  } else {
    n_breaks <- breaks
  }

  # Making histogram
  heights <- hist(x,
    breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE) + IQR(x, na.rm = TRUE) / n_breaks, length = n_breaks),
    right = FALSE,
    plot = FALSE
  )$counts

  # Set up
  pkeys <- scale_num(heights, tonic)
  wave_length <- duration * fs

  # Creating Sounds
  height_notes <- notes(pkeys, dur = duration / n_breaks, fs = fs, inst_lab = inst, progbar = progbar)

  # 4 beat metronome
  snares <- create_count(duration, fs)

  # Adding together
  wave_out <- volume * wave_norm(height_notes + snares)
  return(wave_out)
}

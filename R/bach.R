#' @title Bach Prelude 2
#' @description Generates playable vector of the first half of Prelude 2 (BWV 871) from The Well-Tempered Clavier, Book II
#'
#' @param spd numeric: speed of playback (default: \code{6})
#' @param inst_lab string: instrument used (default:\code{"piano"}).
#' @param want_df logical: if \code{TRUE} returns data frame in long format (default: \code{FALSE})
#' @param stereo logical: if \code{TRUE} returns stereo output, separating notes by hand (default = \code{FALSE})
#'
#' @inheritParams note
#' @return vector or matrix
#' @export
#'
#' @examples
#' \dontrun{
#' wplay_controls <- TRUE
#' wplay(bach(decay = TRUE))
#' }
bach <- function(spd = 6, fs = 44100, inst_lab = "piano", decay = FALSE, decay_rate = -1, want_df = FALSE, stereo = FALSE) {
  # right-hand piano keys (main thread)
  RH1 <- c(
    NA, 47, 45, 47, 48, 45, 43, 45, 47, 43, 42, 43, 45, 42, 40, 42,
    43, 52, 45, 52, 43, 52, 42, 51,
    40, 43, 47, 52, 42, 52, 51, 49, 42, 47, 51, 54, 43, 54, 52, 51,
    43, 48, 52, 55, 45, 55, 54, 52, 45, 50, 54, 57, 47, 57, 55, 54,
    55, 52, 50, 52, 54, 50, 48, 50, 52, 50, 48, 47, 45, NA, NA, NA,
    54, 50, 48, 50, 52, 48, 47, 48, 50, 48, 47, 45, 43, NA, NA, NA,
    52, 48, 47, 48, 50, 47, 45, 47, 48, 45, 43, 45, 48, 50, 48, NA, 48,
    48, 47, 45, 47, 48, 50, 48, NA, 48, 48, 54, 57, 55, 54, 52, 50, 48,
    47, 50, 55, 47, 45, 55, 45, 54, 55, 50, 48, 50, 52, 48, 47, 48,
    50, 47, 45, 47, 48, 45, 43, 45, 47, 43, 42, 43, 40, 48,
    38, 47, 36, 45, 35, 38, 43, 47, 45, 43, 42,
    43, 47, 52, 50, 48, 47
  )

  # right-hand note lengths (1 = 16th note)
  Rd1 <- c(
    rep(1, 16),
    rep(2, 8),
    rep(1, 16),
    rep(1, 16),
    rep(1, 16),
    rep(1, 16),
    rep(1, 12), .25, .25, 1, .5, 2,
    rep(1, 4), .25, .25, 1, .5, 2, rep(1, 8),
    rep(1, 16),
    rep(1, 12), 2, 2,
    rep(2, 4), rep(1, 3), 2, rep(1, 3),
    1, 1, 2, 2, 2, 8
  )

  # last measure multiple voices
  RH2 <- c(NA, 43, 45, NA, 43, 42, 43)
  Rd2 <- c(176, 3, 1, 1, 2, 1, 8)

  RH3 <- c(NA, 38)
  Rd3 <- c(184, 8)


  # LH notes
  LH1 <- c(
    28, 40, 33, 40, 31, 40, 30, 39,
    28, 35, 33, 35, 36, 33, 31, 33, 35, 31, 30, 31, 33, 30, 28, 30,
    31, 28, 33, 30, 35, 33, 35, 31,
    36, 35, 37, 33, 38, 37, 39, 35,
    40, 48, 47, 48, 38, 47, 45, 47, 36, NA, NA, 43, 42, 40,
    38, 47, 45, 47, 36, 45, 43, 45, 35, NA, NA, 41, 40, 38,
    36, 45, 43, 45, 35, 43, 42, 43, 33, 42, 40, 42, 31, 40, 38, 40,
    30, 38, 36, 38, 28, 36, 35, 36, 26, 38, NA, 30,
    31, 28, 24, 26, 19, 31, 24, 31,
    23, 31, 21, 30, 31, 35, 33, 35, 36, 33, 31, 33,
    35, 31, 30, 31, 33, 30, 28, 30, 31, 35, 36, 38,
    40, 36, 38, 26, NA, 31, 27, 23, 19
  )

  # LH lengths
  Ld1 <- c(
    rep(2, 8),
    rep(1, 16),
    rep(2, 8),
    rep(2, 8),
    rep(1, 8), 2, 2, rep(1, 4),
    rep(1, 8), 2, 2, rep(1, 4),
    rep(1, 16),
    rep(1, 8), rep(2, 4),
    rep(2, 8),
    rep(2, 4), rep(1, 8),
    rep(1, 8), rep(2, 4),
    rep(2, 4), rep(1, 4), 4
  )

  # make notes
  R1 <- notes(pkey = RH1, dur = Rd1 / spd, fs = fs, inst_lab = inst_lab, decay = decay, decay_rate = decay_rate)
  R2 <- notes(pkey = RH2, dur = Rd2 / spd, fs = fs, inst_lab = inst_lab, decay = decay, decay_rate = decay_rate)
  R3 <- notes(pkey = RH3, dur = Rd3 / spd, fs = fs, inst_lab = inst_lab, decay = decay, decay_rate = decay_rate)

  L <- notes(pkey = LH1, dur = Ld1 / spd, fs = fs, inst_lab = inst_lab, decay = decay, decay_rate = decay_rate)

  # length discrepancy
  len <- min(length(R1), length(R2), length(R3), length(L))
  R <- R1[1:len] + R2[1:len] + R3[1:len]
  L <- L[1:len]



  prelude2 <- data.frame(
    pkey = c(RH1, RH2, RH3, LH1),
    dur = c(Rd1, Rd2, Rd3, Ld1),
    voices = c(
      rep("RH1", length(Rd1)),
      rep("RH2", length(Rd2)),
      rep("RH3", length(Rd3)),
      rep("LH1", length(Ld1))
    )
  )

  if (want_df) {
    return(prelude2)
  } else if (stereo) {
    return(rbind(L, R))
  } else {
    return(L + R)
  }
}

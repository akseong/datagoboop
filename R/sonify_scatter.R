#' @title Audio scatterplot
#'
#' @description Construct an audio scatterplot.
#'
#' @param data data frame or tibble
#' @param x_var string: name of variable in supplied \code{data} to be used as the independent variable
#' @param y_var string: name of variable in supplied \code{data} to be used as the dependent variable
#' @param tonic_x numeric: piano key number of pitch beginning the scale that \code{x_var} is be mapped to
#' @param tonic_y numeric: piano key number of pitch beginning the scale that \code{y_var} is be mapped to
#' @param n_octaves_x numeric: number of octaves in scale used fo \code{x_var}
#' @param n_octaves_y numeric: number of octaves in scale used fo \code{y_var}
#' @param x_inst string: instrument used to play \code{x_var}
#' @param y_inst string: instrument used to play \code{y_var}
#' @param order_var string: name of variable used to order playback of notes (playback occurs in ascending order of \code{order_var}).  Default: \code{NULL}.  See details.
#' @param factor_var string (in quotes necessary): name of variable in supplied data to be used as factor variable.  Different levels in \code{factor_var} mapped to different instruments used in sonic representation of \code{y_var} (overrides \code{y_inst})
#' @param volume numeric, numeric vector, or name of variable in supplied data: volume of played sounds
#' @param vol_scaling string: scaling to be applied to \code{volume} variable.  See \code{\link{scaling_parser}}
#' @param duration numeric, numeric vector, or name of variable in supplied data: duration of played sounds
#' @param dur_scaling string: scaling to be applied to \code{duration} variable.  See \code{\link{scaling_parser}}
#' @param speed numeric: speed of playback.  See details.
#' @param verbose logical: if \code{TRUE}, generates messages indicating variable mappings and transformations
#' @param stereo logical: if \code{TRUE}, generates stereo output, with \code{x_var} on left ear and \code{y_var} on right.
#' @param under_hood logical: if \code{TRUE}, returns list containing mappings used to construct output (wave vector/matrix is first element)
#' @inheritParams note
#'
#' @return numeric vector: playable audio scatterplot
#' @export
#' @import dplyr
#' @importFrom purrr as_vector
#'
#' @md
#' @details
#'   + \code{order_var}: can specify a different variable in supplied data, "byrow", or, if unspecified (left \code{NULL}), uses \code{x_var} as \code{order_var}.
#'   + variable names for (\code{x_var}, \code{y_var}, \code{order_var}, \code{factor_var}, \code{volume}, \code{duration} need to be entered as quoted strings.
#'   + if \code{speed} left unspecified, defaults to \code{4} (so that \code{duration = 1} plays for 1/4 second instead of 1 second).  However, if \code{speed} unspecified but \code{dur_scaling} specified, then \code{speed} is set to 1.
#'
#'
#' @examples
#' \dontrun{
#' wplay(
#'   sonify_scatter(
#'     data = mtcars,
#'     x_var = "mpg",
#'     y_var = "disp",
#'     factor_var = "gear"
#'   )
#' )
#'
#'
#' # airquality dataset;
#' # order_var = "byrow" since dataset is ordered by day + month
#' wplay_controls <- TRUE
#' wplay(
#'   sonify_scatter(
#'     data = airquality,
#'     x_var = "Ozone",
#'     y_var = "Temp",
#'     order_var = "byrow",
#'     factor_var = "Month",
#'     duration = "Wind",
#'     dur_scaling = "binned 4 exp 2 rev"
#'   )
#' )
#' }
sonify_scatter <- function(data, x_var, y_var, # need to be specified
                           tonic_x = 40, # default is middle C (C4)
                           tonic_y = 52, # default is C5
                           n_octaves_x = 2,
                           n_octaves_y = 2,
                           x_inst = "sine",
                           y_inst = "piano",
                           order_var = NULL,
                           factor_var = NULL, # maps to y instrument (overrides y_inst)
                           volume = NULL, # defaults to 1
                           vol_scaling = NULL, # if volume specified as data var name
                           duration = NULL,
                           dur_scaling = NULL, # if duration specified as data var name
                           speed = NULL,
                           fs = 44100,
                           verbose = TRUE,
                           stereo = FALSE,
                           under_hood = FALSE) {
  # audio scatterplot
  # x_var, y_var values are binned and mapped to pitches
  # tonic_x, tonic_y:  first note of scale used
  # n_octaves_x, n_octaves_y:  number of octaves used in scale

  # grab bare vectors of variables via datamasking
  x <- data %>%
    select({{ x_var }}) %>%
    as_vector()
  y <- data %>%
    select({{ y_var }}) %>%
    as_vector()

  ## optional argument: order_var
  if (is.null(order_var)) {
    ord <- (x)
  } else if (order_var == "byrow") {
    ord <- 1:length(x)
  } else {
    ord <- data %>%
      select({{ order_var }}) %>%
      as_vector()
  }
  ord_inds <- order(ord) # order puts any NA's at the end


  ## optional argument handling: factor_var
  if (is.null(factor_var)) {
    y_inst <- rep(y_inst, length(y))
  } else {
    fac <- data %>%
      select({{ factor_var }}) %>%
      as_vector()
    n_levs <- length(unique(fac))
    if (n_levs > 4) {
      message("Factor has > 4 levels.  Only 4 instruments are implemented (so will be recycled)")
    }
    inst_labs <- c("bass", "piano", "string", "wind") # CHANGE THIS IF MORE INSTRUMENTS ADDED
    inst_labs <- rep(inst_labs, n_levs %/% length(inst_labs) + 1)[1:n_levs] # recycle instruments as needed
    y_inst <- inst_labs[factor(fac)]

    # provide factor key
    factor_map <- distinct(data.frame(y_inst, fac))
    colnames(factor_map) <- c("y_inst", "factor_lev")
    rownames(factor_map) <- NULL
    if (verbose) {
      print("Factor levels mapped to instruments playing y_var:")
      print(factor_map)
    }
  }


  ## optional argument handling: duration (same as volume)
  dur_binned <- FALSE
  if (is.null(duration)) {
    dur <- rep(1, length(x))
  } else {
    if (length(duration) == 1 & is.numeric(duration)) {
      dur <- rep(duration, length(x))
    } else if (length(duration) == 1 & !is.numeric(duration)) {
      # duration specified as data variable
      dur <- data %>%
        select({{ duration }}) %>%
        as_vector()
      dur_obj <- scaling_parser(
        x = dur,
        scaling = dur_scaling,
        msg_varname = "duration variable",
        non_neg = TRUE,
        warn_msgs = verbose
      )
      dur <- dur_obj$scaled
      dur_binned <- dur_obj$binned

      if (is.null(dur_scaling)) {
        print("duration scaled to range [0,1].  Consider specifying dur_scaling")
      }

      if (dur_binned & verbose) {
        print("duration variable binned as follows:")
        print(dur_obj$bin_map)
      }

      if (is.null(speed)) {
        speed <- 1
        if (verbose) {
          message(
            "playback speed set to 1 to match duration scaling. ",
            "Consider specifying speed."
          )
        }
      }
    } else if (length(duration) == length(x) & is.numeric(duration)) {
      dur <- duration
      if (verbose) {
        message("numeric vector supplied for duration; no scaling applied")
      }
    } else {
      stop("duration must be NULL, scalar, data variable name, or numeric vector with length = nrow(dat)")
    }

    # look for NA's, 0's
    durNA <- sum(is.na(dur))
    dur0 <- sum(dur == 0, na.rm = TRUE)
    if (durNA + dur0 > 0) {
      print(paste(
        durNA, "NA's and",
        dur0, "0's in duration variable will not be played"
      ))
    }
    dur[is.na(dur)] <- 0
  }

  # speed it up (4 is a good default if duration = 1 always)
  if (is.null(speed)) {
    speed <- 4
    if (verbose) {
      message(
        "playback speed set to 4",
        "Consider specifying speed."
      )
    }
  }

  ## optional argument handling: volume
  vol_binned <- FALSE
  if (is.null(volume)) {
    vol <- rep(1, length(x))
  } else {
    if (length(volume) == 1 & is.numeric(volume)) {
      vol <- rep(volume, length(x))
    } else if (length(volume) == 1 & !is.numeric(volume)) {
      # volume specified as data variable
      vol <- data %>%
        select({{ volume }}) %>%
        as_vector()
      vol_obj <- scaling_parser(
        x = vol,
        scaling = vol_scaling,
        msg_varname = "volume variable",
        non_neg = TRUE,
        warn_msgs = verbose
      )
      vol <- vol_obj$scaled
      vol_binned <- vol_obj$binned

      if (is.null(vol_scaling)) {
        print("volume scaled to range [0,1].  Consider specifying vol_scaling")
      }

      if (vol_binned & verbose) {
        print("volume variable binned as follows:")
        print(vol_obj$bin_map)
      }
    } else if (length(volume) == length(x) & is.numeric(volume)) {
      vol <- volume
      if (verbose) {
        message("numeric vector supplied for volume; no scaling applied (max volume = 1)")
      }
    } else {
      stop("volume must be NULL, scalar, data variable name, or numeric vector with length = nrow(dat)")
    }

    # look for NA's, 0's
    volNA <- sum(is.na(vol))
    vol0 <- sum(vol == 0, na.rm = TRUE)
    if (volNA + vol0 > 0) {
      print(paste(
        volNA, "NA's and",
        vol0, "0's in volume variable represented as silence"
      ))
    }
    vol[is.na(vol)] <- 0
  }


  ## main argument error handling
  if (!is.numeric(x) | !is.numeric(y) | !is.numeric(ord)) {
    stop("x_var, y_var, (and order_var if specified) must be numeric")
  }

  if (sum(is.na(x)) > 0 | sum(is.na(y)) > 0) {
    message(
      sum(is.na(x)), " NAs in x_var and ",
      sum(is.na(y)), " NAs in y_var ",
      "represented by silence in corresponding voice"
    )
  }

  if (sum(is.na(ord)) > 0) {
    message("NAs in order_var will not be played at all")
  }


  ## create pitches and sound waves
  # map data to pkey pitches
  x_pitch <- scale_num(x, tonic_pkey = tonic_x, n_octaves = n_octaves_x)
  y_pitch <- scale_num(y, tonic_pkey = tonic_y, n_octaves = n_octaves_y)

  # trim out NAs in order_var
  ord_inds <- ord_inds[!is.na(ord[ord_inds])]

  # turn ordered pitches into .wav-playable vectors
  x_notes <- notes(
    pkey = x_pitch[ord_inds],
    vol = .75 * vol[ord_inds],
    dur = (dur[ord_inds] / speed),
    fs = fs,
    inst_lab = x_inst
  )

  y_notes <- notes(
    pkey = y_pitch[ord_inds],
    vol = vol[ord_inds],
    dur = (dur[ord_inds] / speed),
    fs = fs,
    inst_lab = y_inst[ord_inds]
  )

  if (stereo) {
    min_len <- min(length(x_notes), length(y_notes))
    wmat <- rbind(
      x_notes[1:min_len],
      y_notes[1:min_len]
    )
    wave <- wave_norm(wmat)
  } else {
    wave <- wave_norm(x_notes + y_notes)
  }

  # warn user of playback time
  play_time <- length(x_notes) / fs
  message(
    "At speed = ", speed,
    " playback will take ", round(play_time, 2), " seconds"
  )

  # optional return - get wave vector, playback time, and dataframe of mappings
  if (under_hood) {
    sound_df <- data.frame(
      x_pitch = x_pitch,
      y_pitch = y_pitch,
      x_volume = .75 * vol,
      y_volume = vol,
      duration = dur,
      y_instruments = y_inst,
      order = ord_inds
    )

    res <- list(
      wave = wave,
      playback_time = play_time,
      sound_df = sound_df
    )

    # add mappings of factor levels / durations when binned
    if (!is.null(factor_var)) res[["factor_map"]] <- factor_map
    if (dur_binned) res[["dur_bins"]] <- dur_obj$bin_map
    if (vol_binned) res[["vol_bins"]] <- vol_obj$bin_map

    return(res)
  }

  return(wave)
}

#' @title Interpret scaling string (helper function / Internal use)
#'
#' @description Helper function used in \code{sonify_scatter()}.  Uses \code{stringr} functions to parse \code{scaling} string.
#' @param x numeric vector to be scaled
#' @param scaling string: description of desired scaling of data to pitch
#' @param msg_varname string: name of variable \code{x} to be used in messages
#' @param non_neg logical: if \code{TRUE}, scaled version of \code{x} (function output) is transformed to be non-negative
#' @param warn_msgs logical: if \code{TRUE} (default), prints messages for each scaling step
#'
#' @return list containing original \code{x}, scaled \code{x}, and other indicators/diagnostics
#' @export
#' @importFrom stringr str_extract
#'
#' @md
#' @details uses \code{str_detect()} to parse \code{scaling} strings.  Looks for (in order):
#'
#' 1. \code{bin} OR \code{quant}: place in equally-spaced bins OR bin by quantile.
#'    + If "bin" or "quant" detected, looks for integer as # of bins.
#'    + Integer must be after "bin" or "quant" but before "exp" (if "exp" specified); defaults to 5.
#' 1. \code{rev}: reverse magnitude while keeping same range
#' 1. \code{exp}: use variable as exponent
#'    + if "exp" detected, looks for integer/decimal to be the base (defaults to 2)
#'
#' Mostly makes sense to use this function for volume and duration variables
#' (enforces non-negativity, reports 0's and NA's)
#'
#' @examples
#' \dontshow{wplayback <- FALSE}
#' scaling_parser(x = airquality$Ozone, scaling = "binned 6 exp 1.5")
#'
scaling_parser <- function(x, scaling = NULL,
                           msg_varname = "x",
                           non_neg = TRUE,
                           warn_msgs = TRUE) {

  # uses str_detect() to parse `scaling` strings.
  # looks for (in order):
  #    - bin OR quant: place in equally-spaced bins OR bin by quantile
  #          - if "bin" or "quant" detected, looks for integer as # of bins
  #            (integer must be after pattern but before "exp" if specified; defaults to 5)
  #    - rev: reverse magnitude while keeping same range
  #    - exp: use variable as exponent
  #          - if "exp" detected, looks for integer/decimal to be the base (defaults to 2)
  # mostly makes sense to use this function for volume and duration variables
  # (enforces non-negativity, reports 0's and NA's)
  #


  if (sum(x == 0, na.rm = TRUE) == length(!is.na(x))) stop(paste(msg_varname, "is all 0's or NA's"))

  x_temp <- x
  # no scaling specified AND
  # either x is non-negative OR not required to be non-neg
  if (is.null(scaling) & (!non_neg | sum(x < 0, na.rm = TRUE) == 0)) {
    n_NA <- sum(is.na(x))

    if (warn_msgs & (n_NA > 0)) {
      message(n_NA, " NA's in ", msg_varname)
    }

    x_temp <- x / max(abs(x))

    if (warn_msgs) {
      print(paste0(msg_varname, " scaled to range [-1, 1]"))
    }

    res <- list(
      "orig" = x,
      "scaled" = x_temp,
      "binned" = FALSE,
      "n_0" = sum(x == 0, na.rm = TRUE),
      "n_NA" = n_NA
    )
    return(res)
  }

  # do binning (if desired) first
  if (stringr::str_detect(scaling, "bin")) {
    # grab first number after bin but before exp (if exists)
    n_bin_string <- strsplit(scaling, "bin")[[1]][2]
    n_bin_string <- strsplit(n_bin_string, "exp")[[1]][1]

    n_bins <- as.numeric(stringr::str_extract(n_bin_string, "[:digit:]+"))
    n_bins <- ifelse(!is.na(n_bins), n_bins, 5) # defaults to 5 bins
    x_bins <- cut(x, breaks = n_bins)
    x_temp <- as.numeric(x_bins)
    print(paste0(msg_varname, " binned into ", n_bins, " equally spaced bins"))
    binned <- TRUE
  } else if (stringr::str_detect(scaling, "quant")) {
    # grab first number after quant but before exp (if exists)
    n_bin_string <- strsplit(scaling, "quant")[[1]][2]
    n_bin_string <- strsplit(n_bin_string, "exp")[[1]][1]

    n_bins <- as.numeric(stringr::str_extract(n_bin_string, "[:digit:]+"))
    n_bins <- ifelse(!is.na(n_bins), n_bins, 5) # defaults to 5 bins
    x_bins <- cut(x,
      breaks = stats::quantile(x, probs = (0:n_bins) / n_bins, na.rm = TRUE),
      include.lowest = TRUE
    )
    x_temp <- as.numeric(x_bins)
    print(paste0(msg_varname, " binned into ", n_bins, " equally spaced quantiles"))
    binned <- TRUE
  } else {
    binned <- FALSE
  }

  # reverse scaled value magnitude
  if (stringr::str_detect(scaling, "rev")) {
    x_temp <- sum(range(x_temp, na.rm = TRUE)) - x_temp
    if (warn_msgs) print(paste(msg_varname, "magnitudes reversed"))
  }

  # exponentiate.  Changes in volume and duration more naturally detected
  if (stringr::str_detect(scaling, "exp")) {
    # grab first number (integer or decimal) after exp (if exists)
    after_exp <- strsplit(scaling, "exp")[[1]][2]
    base_spec <- as.numeric(
      str_extract(
        after_exp,
        "[+-]?([1-9]\\d*(\\.\\d*[1-9])?|0\\.\\d*[1-9]+)|\\d+(\\.\\d*[1-9])?"
      )
    )
    exp_base <- ifelse(!is.na(base_spec), base_spec, 2) # default base of 2
    print(paste0(msg_varname, " transformed: ", exp_base, "^", msg_varname))
    x_temp <- exp_base^(x_temp) # base 2 --> 16th notes, quarter notes, half, etc.
  }

  # if variable must be non-neg (but isn't)
  if (non_neg & sum(x_temp < 0, na.rm = TRUE) > 0) {
    x_temp <- abs(x)
    if (warn_msgs) {
      message(msg_varname, " must be non-negative (transformed to abs(x))")
    }
  }

  # 0's and coerce NA to 0
  n_0 <- sum(x_temp == 0, na.rm = TRUE)
  n_NA <- sum(is.na(x_temp))
  if (warn_msgs & n_0 > 0) {
    message(n_0, " 0's in scaled ", msg_varname)
  } else if (warn_msgs & n_NA > 0) {
    message(n_NA, " NA's in scaled ", msg_varname, "coerced to 0")
  }
  x_temp[is.na(x_temp)] <- 0

  # scale to [-1, 1]
  x_temp <- x_temp / max(abs(x_temp), na.rm = TRUE)

  if (non_neg & warn_msgs) {
    print(paste0(msg_varname, " scaled to range [0, 1]"))
  } else if (!non_neg & warn_msgs) {
    print(paste0(msg_varname, " scaled to range [-1, 1]"))
  }

  res <- list(
    "orig" = x,
    "scaled" = x_temp,
    "binned" = binned,
    "n_0" = n_0,
    "n_NA" = n_NA
  )
  if (binned) {
    bin_map <- distinct(data.frame(x_bins, x_temp))
    colnames(bin_map) <- c(paste0(msg_varname, "_bins"), "scaled")
    rownames <- NULL
    res[["bin_map"]] <- list(bin_map)
  }
  return(res)
}

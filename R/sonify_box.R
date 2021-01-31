#' @title   Sonified Boxplot
#' @description  A sound will slide pitchwise along the distribution of the data.
#' You can compare distributions by comparing the range of pitches being traveled.
#'
#' @inheritParams sonify_hist
#' @param var : The variable to summarize with the sonified boxplot
#' @param group_var : If comparing distributions: the variable that splits the dataset into groups.
#' @param phase_speed : numeric: the number of phases per second for each group.
#'
#' @return 2xn_samples matrix for playback.
#' @export
#' @importFrom grDevices boxplot.stats
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(x = rnorm(100))
#' wplay(sonify_box(dat, x))
#' }
sonify_box <- function(data,
                       var,
                       group_var = NULL,
                       tonic = 40,
                       duration = NULL,
                       phase_speed = 0.5,
                       volume = 0.75,
                       fs = 44100,
                       progbar=TRUE) {

  # Summarizing the data

  sum_tab <- data %>%
    mutate(var = scale({{ var }})) %>%
    group_by({{ group_var }}) %>%
    summarise(
      min = boxplot.stats({{ var }})$stats[1],
      mean = mean({{ var }}),
      max = boxplot.stats({{ var }})$stats[5]
    ) %>%
    mutate(range = max - min) %>%
    mutate(range = 20 * range / max(range))

  if (dim(sum_tab)[1] > 1) {
    writeLines(paste("The playing order for the grouping variable are \n ", print(sum_tab[, 1])))
  }



  k <- nrow(sum_tab)

  if (is.null(duration)) {
    duration <- k * 2
  }

  # initializiing waves in stereo
  l_wave <- rep(0, fs * duration)
  r_wave <- l_wave
  left_pan <- 1 - seq(0, 1, length.out = k)
  if (k == 1) {
    left_pan <- 0.5
  }

  # Initializing progress bar
  if (progbar) {pb <- txtProgressBar(min = 0, max = k, style = 3, title = progbar_title)}


  # Creating the boxplot
  for (i in 1:k) {
    l_wave <- l_wave +
      c(
        rep(0, floor(fs * (i - 1) * duration / k)),
        rep(
          left_pan[i] *
            wurr(
              hz = pkey_to_hz(tonic + sum_tab$mean[i]),
              phase_size = sum_tab$range[i],
              phase_speed = phase_speed,
              dur = duration / k,
              fs = fs
            ),
          k - i + 1
        )
      )
    r_wave <- r_wave +
      c(
        rep(0, floor(fs * (i - 1) * duration / k)),
        rep(
          (1 - left_pan[i]) *
            wurr(
              hz = pkey_to_hz(tonic + sum_tab$mean[i]),
              phase_size = sum_tab$range[i],
              phase_speed = phase_speed,
              dur = duration / k,
              fs = fs
            ),
          k - i + 1
        )
      )

    if (progbar) {setTxtProgressBar(pb, i)}
  }
  wave_out <- wave_norm(rbind(l_wave, r_wave))
  return(volume * wave_out)
}

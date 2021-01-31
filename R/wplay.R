
#' @title Playback an audio vector
#' @description playback with system wait (default) or console controls.  When knitting .Rmd file to html, saves audio as .wav file and places html audio player in document.
#'
#' @param y either a n_samples length vector or a 2xn_samples matrix
#' @param vol numeric in \[0, 1\]: playback volume (default 0.5)
#' @param normalize logical: if `TRUE` (default) audio is normalized before playback.
#' @param fs integer: playback sampling rate (user system-dependent; typically 44100 or 48000)
#' @param file_path string: location/filename to save audio file when knitting to an html document
#' @param file_type string: file type of local copy when knitting html document (wav/mp3)
#' @param wplayback logical: if `TRUE` the audio is played.
#' @param wplay_controls logical: if `TRUE` the audio is played using in-console controls.  If `FALSE` places system in sleep during playback.
#' @param eval_entry logical: if \code{TRUE} and console controls active, executes console input other than 'p' (pause/resume) or 's' (stop)}
#' @param n_sec numeric: number of seconds to play from beginning or end
#'
#' @md
#' @details
#'   + \code{vol}: for these functions, \code{vol} is applied to the soundwave _after_ normalization (i.e. acts like a master volume for playback).
#'   + \code{file_path}: if unspecified, creates folder with same name as current .Rmd file and saves audio file using \[timestamp\].\[\code{file_type}\] as the filename.
#'   + \code{wplayback} can be set in parent environment / in console, i.e. can toggle audio playback on/off by creating a new variable \code{wplayback <- TRUE} or \code{FALSE}.   Useful when sourcing file / running all lines before, et cetera.
#'   + \code{wplay_controls} can be set in parent environment / in console.
#'   + \code{eval_entry} can be set in parent environment / in console.
#'
#' @return If called from script or in console, plays audio.  If knitting .Rmd document to html, saves sound file and inserts html audio tags into document
#' @export
#' @importFrom audio play pause resume
#'
#' @examples
#' \dontshow{
#' wplayback <- FALSE
#' }
#' x <- sin(1:100000 / (3 * pi))
#' wplay(x)
#'
#' # two notes
#' y <- sin(1:100000 / (5 * pi))
#' wplay(x + y)
#'
#' # no playback
#' wplay(x + y, wplayback = FALSE)
#' wplayback <- FALSE
#' wplay(x)
#'
#' # use parent environment to toggle console playback controls
#' wplayback <- TRUE
#' \dontshow{
#' wplayback <- FALSE
#' }
#' wplay_controls <- TRUE
#' \dontshow{
#' wplay_controls <- FALSE
#' }
#' z <- sin(1:441000 / (3 * pi)) # playback takes 10 seconds.  Use console controls to stop.
#' wplay(z)
wplay <- function(y, vol = 0.5, normalize = TRUE, fs = 44100,
                  file_path = NULL,
                  file_type = "wav",
                  wplayback,
                  wplay_controls,
                  eval_entry) {

  # if wplayback not defined as argument, function looks for wplayback in parent frame
  # if can't find in parent frame, wplayback set to TRUE
  if (missing(wplayback)) {
    par_envir <- parent.frame()

    if ("wplayback" %in% ls(par_envir)) {
      wplayback <- par_envir$wplayback
      message(paste0(
        "wplayback declared to be ",
        wplayback,
        " in parent environment \n"
      ))
    } else {
      wplayback <- TRUE
    }
  }

  if (missing(wplay_controls)) {
    par_envir <- parent.frame()

    if ("wplay_controls" %in% ls(par_envir)) {
      wplay_controls <- par_envir$wplay_controls
      message(paste0(
        "wplay_controls declared to be ",
        wplay_controls,
        " in parent environment \n"
      ))
    } else {
      wplay_controls <- FALSE
    }
  }

  if (missing(eval_entry)) {
    par_envir <- parent.frame()
    
    if ("eval_entry" %in% ls(par_envir)) {
      eval_entry <- par_envir$eval_entry
      message(paste0(
        "eval_entry declared to be ",
        eval_entry,
        " in parent environment \n"
      ))
    } else {
      eval_entry <- FALSE
    }
  }
  if (normalize) y <- wave_norm(y)

  # vol = 0.25 by default to protect your ears.
  if (vol >= 0 & vol <= 1) {
    y <- vol * y
  } else {
    stop("vol must be between 0 and 1.")
  }



  # detects if source is currently in the process of being knit (NULL if just run in console)
  knitting <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))
  # detects if file will be knit to html (not sure if this still works non .Rmd file)
  htmlknit <- knitr::is_html_output()

  if (knitting & htmlknit) {
    # if knitting to html, save file (locally) and insert html audio player

    if (is.null(file_path)) {
      # create a (most likely) unique name for the file,
      # but which won't change unless the audio snippet does

      # create subdirectory in folder to hold .wav files.
      # if subdir exists, won't output warning message
      dir.create(
        file.path(stringr::str_sub(knitr::current_input(), end = -5)),
        showWarnings = FALSE
      )

      file_path <- paste0(
        stringr::str_sub(knitr::current_input(), end = -5),
        "/",
        format(Sys.time(), "%Y-%m-%d_%Hh%Mm%OS4"),
        ".",
        file_type
      )
    }

    # save file locally to load into html
    seewave::savewav(y, f = fs, file = file_path)
    html_tag_audio(file_path = file_path)
  } else if (wplayback) {
    if (wplay_controls) {
      cplay(
        y = y, 
        vol = vol, 
        normalize = FALSE, 
        fs = fs, 
        eval_entry = eval_entry
      )
    } else {
      audio::wait(audio::play(y, rate = fs))
    }
  }
}




#' @describeIn wplay Audio playback with console controls (type "p" to pause, "r" to resume, "s" to stop).
#' @export
cplay <- function(y, vol = 0.5, normalize = TRUE, fs = 44100, eval_entry) {
  # implements playback controls inside console
  
  # check for eval_entry in parent environment
  if (missing(eval_entry)) {
    par_envir <- parent.frame()
    
    if ("eval_entry" %in% ls(par_envir)) {
      eval_entry <- par_envir$eval_entry
      message(paste0(
        "eval_entry declared to be ",
        eval_entry,
        " in parent environment \n"
      ))
    } else {
      eval_entry <- FALSE
    }
  }
  
  # Normalize the input by default
  if (normalize) {
    y <- wave_norm(y)
  }

  # vol = 0.25 by default to protect your ears.
  if (vol >= 0 & vol <= 1) {
    y <- vol * y
  } else {
    stop("vol must be between 0 and 1.")
  }

  t_left <- length(y) / fs # remaining time (in seconds)
  if (is.matrix(y)) {
    t_left <- t_left / min(dim(y)) # assumes # channels (i.e. stereo = 2) lower than length
  }
  
  if (eval_entry){
    cat(paste0(
      "\033[0;1;36m",
      "Entries other than 'p' or 's' will be executed in the global environment (single line only)",
      "\033[0m","\n"))
  } else {
    message("Entries other than 'p' or 's' will stop playback")
  }
  
  key <- NULL
  playing <- TRUE
  
  out <- audio::play(y, rate = fs) # start playing sound
  t_st <- Sys.time() # start time

  while (t_left > 0) {
    key <- readline("'p' + [Enter] to pause/resume, s' + [Enter] to stop:  ")
    if (key != "p"){
      # while loop stop condition
      t_left <- -1   
    } else if (key == "p") {
      # pause/resume
      if (playing){
        # pause
        t_now <- Sys.time()
        audio::pause(out)
        t_left <- t_left - difftime(t_now, t_st, units = "secs") # increment to stop
        if (t_left > 0) {
          print(paste0(round(t_left, 2), " sec remaining"))
        }
        playing <- FALSE
      } else {
        #resume
        audio::resume(out)
        t_st <- Sys.time()
        playing <- TRUE
      }
    } 
  }
  audio::close.audioInstance(out)
  if (t_left == -1){
    print("playback stopped")  
  } else {
    print("playback finished")
  }
  
  if (key != "s" & key !="p" & eval_entry) {
    # evaluate readline input
    cat(paste0(
      "Input ",
      "\033[0;1;36m",
      "'", key, "'",
      "\033[0m",
      "executed in global environment",
      "\n"))
    eval(parse(text=key), envir=.GlobalEnv)        
  }
}





#' @describeIn wplay Plays the first n_sec seconds of y
#' @export
wplay_head <- function(y, vol = 0.5, n_sec = 3,
                       normalize = TRUE, fs = 44100,
                       file_path = NULL,
                       file_type = "wav") {

  # plays first n_sec of y (wrapper for wplay)
  len <- length(y)
  y_new <- y[1:min(len, n_sec * fs)]
  wplay(y_new,
    vol = vol, normalize = normalize, fs = fs,
    file_path = file_path,
    file_type = file_type
  )
}


#' @describeIn wplay Plays the last n_sec seconds of y
#' @export
wplay_tail <- function(y, vol = 0.5, n_sec = 3,
                       normalize = TRUE, fs = 44100,
                       file_path = NULL,
                       file_type = "wav") {

  # plays last n_sec of y (wrapper for wplay)
  len <- length(y)
  y_new <- y[max(1, len - n_sec * fs):len]
  wplay(y_new,
    vol = vol, normalize = normalize, fs = fs,
    file_path = file_path,
    file_type = file_type
  )
}

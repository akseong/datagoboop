
#' @title Playback an audio vector
#' @description playback with system wait (default) or console controls.  When knitting .Rmd file to html, saves audio as .wav file and places html audio player in document.
#' 
#' @param y either a n_samples length vector or a 2xn_samples matrix
#' @param vol numeric in \[0, 1\]: playback volume (default 0.25) 
#' @param normalize logical: if `TRUE` (default) audio is normalized before playback.
#' @param fs integer: playback sampling rate (user system-dependent; typically 44100 or 48000)
#' @param file_path string: location/filename to save audio file when knitting to an html document
#' @param file_type string: file type of local copy when knitting html document (wav/mp3)
#' @param wplayback logical: if `TRUE` the audio is played.  
#' @param wplay_controls logical: if `TRUE` the audio is played using in-console controls.  If `FALSE` places system in sleep during playback.  
#' @param n_bad integer: number of invalid responses to console prompts before stopping.
#' @param n_sec numeric: number of seconds to play from beginning or end 
#' 
#' @md
#' @details  
#'   + \code{vol}: for these functions, \code{vol} is applied to the soundwave _after_ normalization (i.e. acts like a master volume for playback).
#'   + \code{file_path}: if unspecified, creates folder with same name as current .Rmd file and saves audio file using \[timestamp\].\[\code{file_type}\] as the filename.
#'   + \code{wplayback} can be set in parent environment / in console, i.e. can toggle audio playback on/off by creating a new variable \code{wplayback <- TRUE} or \code{FALSE}.   Useful when sourcing file / running all lines before, et cetera.
#'   + \code{wplay_controls} can be set in parent environment / in console.
#' 
#' @return If called from script or in console, plays audio.  If knitting .Rmd document to html, saves sound file and inserts html audio tags into document
#' @export
#' @importFrom audio play pause resume
#'
#' @examples
#' \dontshow{wplayback = FALSE}
#' x <- sin(1:100000/(3*pi))
#' wplay(x)
#' 
#' # two notes
#' y <- sin(1:100000/(5*pi))
#' wplay(x + y)
#' 
#' # no playback
#' wplay(x + y, wplayback = FALSE)
#' wplayback = FALSE
#' wplay(x)
#' 
#' # use parent environment to toggle console playback controls 
#' wplayback = TRUE
#' \dontshow{wplayback = FALSE}
#' wplay_controls = TRUE
#' \dontshow{wplay_controls = FALSE}
#' z <- sin(1:441000/(3*pi))  # playback takes 10 seconds.  Use console controls to stop.
#' wplay(z)
wplay <- function(y, vol = 0.25, normalize = TRUE, fs = 44100,
                  file_path = NULL, 
                  file_type = "wav",
                  wplayback, 
                  wplay_controls){
  
  # if wplayback not defined as argument, function looks for wplayback in parent frame
  # if can't find in parent frame, wplayback set to TRUE
  if (missing(wplayback)){
    par_envir <- parent.frame()
    
    if ("wplayback" %in% ls(par_envir)){
      wplayback <- par_envir$wplayback
      message(paste0(
        "wplayback declared to be ",
        wplayback, 
        " in parent environment \n")
      )
    } else {
      wplayback <- TRUE
    }
  }
  
  if (missing(wplay_controls)){
    par_envir <- parent.frame()
    
    if ("wplay_controls" %in% ls(par_envir)){
      wplay_controls <- par_envir$wplay_controls
      message(paste0(
        "wplay_controls declared to be ",
        wplay_controls, 
        " in parent environment \n")
      )
    } else {
      wplay_controls <- FALSE
    }
  }
  
  if (normalize) y <- wave_norm(y)
  
  # vol = 0.25 by default to protect your ears.
  if (vol >=0 & vol <= 1){
    y <- vol * y
  } else {
    stop("vol must be between 0 and 1.")
  }
  
  
  
  # detects if source is currently in the process of being knit (NULL if just run in console)
  knitting <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))
  # detects if file will be knit to html (not sure if this still works non .Rmd file)
  htmlknit <- knitr::is_html_output()
  
  if (knitting & htmlknit){
    # if knitting to html, save file (locally) and insert html audio player
    
    if (is.null(file_path)){
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
      cplay(y = y, vol = 1, normalize = FALSE, fs = fs)
    } else {
      audio::wait(audio::play(y, rate = fs))
    }
    
  }
  
}




#' @describeIn wplay Audio playback with console controls (type "p" to pause, "r" to resume, "s" to stop).
#' @export
cplay <- function(y, vol = 0.25, normalize = TRUE, fs = 44100, n_bad = 5){
  # implements playback controls inside console
  
  # Normalize the input by default
  if (normalize) {
    y <- wave_norm(y)
  }
  
  # vol = 0.25 by default to protect your ears.
  if (vol >=0 & vol <= 1){
    y <- vol * y
  } else {
    stop("vol must be between 0 and 1.")
  }
  
  
  t_left = length(y) / fs         # remaining time (in seconds)
  if (is.matrix(y)) {
    t_left = t_left / min(dim(y)) # assumes # channels (i.e. stereo = 2) lower than length 
  }
  key <- NULL
  playing <- TRUE
  
  t_st <- Sys.time()                   # start time
  out <- audio::play(y, rate = fs)            # start playing sound
  message("press 's' + [Enter] to exit/stop playback at any time.")
  n_badkeys <- 0            # panic button.  counts number invalid inputs before stops playback
  
  while (t_left > 0) {
    if (n_badkeys >= n_bad) {
      message("panic button hit: invalid entries > ", n_bad-1, "; playback/prompts stopped" )
      t_left <- -1
      next
    }
    
    
    # options to pause and stop (playing = TRUE)
    if (playing & t_left > 0){
      key <- readline("'p' + [Enter] to pause, 's' + [Enter] to stop:  ")
      if (key == "p"){
        audio::pause(out)
        playing = FALSE      # toggles looking for "r" instead of "p"
        t_now <- Sys.time()
        t_left <- t_left - difftime(t_now, t_st, units = "secs") # increment to stop
        if (t_left > 0){
          print(paste0(round(t_left, 2), " sec remaining"))
        }
      } else if (key == "s"){
        print("playback stopped")
        audio::close.audioInstance(out)
        t_left <- -1               # stops while loop
      } else {
        n_badkeys <- n_badkeys + 1 # increment to stopping condition 
      }
      next
    }
    
    # options to resume (playing = FALSE) and stop
    if (!playing & t_left > 0){
      key <- readline("'r' + [Enter] to resume, 's' + [Enter] to stop:  ")
      if (key == "r"){
        audio::resume(out)
        playing = TRUE
        t_st <- Sys.time()
      } else if (key == "s"){
        audio::close.audioInstance(out)
        print("playback stopped")
        t_left <- -1               # stopping condition 
      } else {
        n_badkeys <- n_badkeys + 1 # increment to stopping condition 
      }
      next
    }
    
  }
  audio::close.audioInstance(out) 
  if (key != "s") print("finished")
}



#' @describeIn wplay Plays the first n_sec seconds of y
#' @export
wplay_head <- function(y, vol = 0.25, n_sec=3, 
                       normalize = TRUE, fs = 44100,
                       file_path = NULL, 
                       file_type = "wav"){
  
  # plays first n_sec of y (wrapper for wplay)
  len <- length(y)
  y_new <- y[1:min(len, n_sec*fs)]
  wplay(y_new, vol = vol, normalize = normalize, fs = fs,
        file_path = file_path, 
        file_type = file_type)
}


#' @describeIn wplay Plays the last n_sec seconds of y
#' @export
wplay_tail <- function(y, vol = 0.25, n_sec=3, 
                       normalize = TRUE, fs = 44100,
                       file_path = NULL, 
                       file_type = "wav"){
  
  # plays last n_sec of y (wrapper for wplay)
  len <- length(y)
  y_new <- y[max(1, len - n_sec*fs):len]
  wplay(y_new, vol = vol, normalize = normalize, fs = fs,
        file_path = file_path, 
        file_type = file_type)
}


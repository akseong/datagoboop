#' Create audio tag (helper function / Internal Use)
#'
#' Used in \code{wplay()} when knitting an .Rmd file to html.  Inserts audio tags / creates audio player within document.
#' @param file_path The path of the audio file
#' @param type The file type (wav/mp3)
#'
#' @return html audio tag
#' @export
#' 
#' @examples
#' \dontrun{
#' html_tag_audio("foo")
#' }
html_tag_audio <- function(file_path, type = "wav") {
  # code from https://community.rstudio.com/t/audio-files-in-r-markdown/20874/3
  htmltools::tags$audio(
    controls = "",
    htmltools::tags$source(
      src = file_path,
      type = paste0("audio/", type)
    )
  )
}

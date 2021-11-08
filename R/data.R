#' Parameters for preset instrument sounds
#'
#' A dataset used to synthesize four instrument sounds: piano, bass, wind, and
#' string. The data represent temporal envelope, relative amplitudes, and phase
#' offsets of harmonic components in each instrument sound. 
#'
#' @format A list containing four sub-lists. Each sub-list represents the 
#' parameters for a unique instrument.
#' 
#' Each sub-list contains three elements: \code{env}, \code{amp_f}, and \code{phi_f}.
#' These represent the temporal envelope, the base level of each harmonically
#' related frequency, and the phase offset of each harmonically related frequency,
#' respectively. These parameters are used by DataGoBoop's \code{resynth} function
#' to create sound.
#' \describe{
#'   \item{inst$piano}{List used to synthesize a piano-like sound}
#'   \item{inst$bass}{List used to synthesize a bass-guitar-like sound}
#'   \item{inst$wind}{List used to synthesize a flute-like sound}
#'   \item{inst$string}{List used to synthesize a violin-like sound}
#' }
#' @source These data were generated manually using the \code{add_inst} function
#' and representative samples of each kind of instrument.
"inst"
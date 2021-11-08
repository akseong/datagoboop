#' Parameters to synthesize 4 instrument sounds.
#'
#' A dataset used to synthesize instrument sounds. The data include temporal
#' envelope, relative amplitudes, and phase offsets of harmonic components in 
#' each instrument sound. 
#'
#' @format A list containing 4 sub-lists, each representing a unique instrument.
#' Each sub-list contains 3 elements, env, amp_f, and phi_f, which represent
#' the temporal envelope, the base level of each harmonically related frequency,
#' and the phase offset of each harmonically related frequency, respectively.
#' \describe{
#'   \item{piano}{used to synthesize a piano-like sound}
#'   \item{bass}{used to synthesize a bass-guitar-like sound}
#'   \item{wind}{used to synthesize a flute-like sound}
#'   \item{string}{used to synthesize a violin-like sound}
#' }
#' @source These data were generated manually using the \code{add_inst} function
#' and representative samples of each kind of instrument.
"instruments"
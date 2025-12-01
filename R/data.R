################################################################################
# Backchannel Detection Word Lists
# 
# Word lists for classifying overlapping speech as backchannels vs secondary
# turns, based on Cooney & Reece (2025).
################################################################################

#' Backchannel Cue Words
#'
#' A character vector of words that are typically used as backchannel cues
#' (e.g., "yeah", "okay", "mhm", "right"). Used by the backchannel detection
#' algorithm to classify overlapping speech.
#'
#' @format A character vector of 31 words
#'
#' @source Cooney & Reece (2025). Scientific Reports.
#'   \url{https://doi.org/10.1038/s41598-025-24381-1}
#'
#' @keywords datasets
#' @export
BACKCHANNEL_CUES <- c(
  "a", "ah", "alright", "awesome", "cool", "dope", "e", "exactly",
  "god", "gotcha", "huh", "hmm", "mhm", "mm hmm", "mm", "mmm",
  "nice", "oh", "okay", "really", "right", "sick", "sucks", "sure",
  "uh", "um", "wow", "yeah", "yep", "yes", "yup"
)

#' Not Backchannel Cue Words
#'
#' A character vector of words that, when used at the start of an utterance,
#' indicate it is NOT a backchannel but rather a secondary turn (e.g., "and",
#' "but", "i", "well"). Used by the backchannel detection algorithm.
#'
#' @format A character vector of 15 words
#'
#' @source Cooney & Reece (2025). Scientific Reports.
#'   \url{https://doi.org/10.1038/s41598-025-24381-1}
#'
#' @keywords datasets
#' @export
NOT_BACKCHANNEL_CUES <- c(
  "and", "but", "i", "i'm", "it", "it's", "like", "so",
  "that", "that's", "we", "we're", "well", "you", "you're"
)

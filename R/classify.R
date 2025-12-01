################################################################################
# Turn Classification Functions
# 
# Classify turns as primary/secondary/backchannel using Python's algorithm
################################################################################

#' Determine Utterance Type
#'
#' Classifies an overlapping utterance as "backchannel", "secondary", or "other"
#' based on word count, duration, and word lists. Implements the backchannel
#' detection algorithm from the Python NaturalTurn implementation.
#'
#' @param utterance_text Character string containing the utterance text.
#' @param n_words Integer, number of words in the utterance.
#' @param duration Numeric, duration of the utterance in seconds.
#' @param backchannel_word_max Maximum word count for backchannel classification.
#'   Default: 3.
#' @param backchannel_second_max Maximum duration (seconds) for backchannel
#'   classification. Default: 0.0 (effectively disabled).
#' @param backchannel_proportion Minimum proportion of words that must be
#'   backchannel cues. Default: 0.5 (50%).
#'
#' @return Character string: "backchannel", "secondary", or "other".
#'
#' @details Classification rules (applied in order):
#' \itemize{
#'   \item If EVERY word is a backchannel cue → BACKCHANNEL (any length)
#'   \item If >backchannel_word_max words AND >backchannel_second_max duration → SECONDARY
#'   \item If starts with NOT_BACKCHANNEL_CUE → SECONDARY
#'   \item If ≥backchannel_proportion words are backchannel cues → BACKCHANNEL
#'   \item If ≤backchannel_word_max words → BACKCHANNEL
#'   \item Otherwise → SECONDARY
#' }
#'
#' @importFrom stringr str_split str_replace_all
#' @keywords internal
determine_utterance_type <- function(utterance_text,
                                     n_words,
                                     duration,
                                     backchannel_word_max = 3,
                                     backchannel_second_max = 0.0,
                                     backchannel_proportion = 0.5) {

  # Handle empty utterances
  if (is.na(utterance_text) || nchar(trimws(utterance_text)) == 0 || n_words == 0) {
    return("other")
  }

  # Tokenize utterance (split on whitespace, remove punctuation for matching)
  tokens <- stringr::str_split(tolower(utterance_text), "\\s+")[[1]]
  tokens <- tokens[nchar(tokens) > 0]  # Remove empty strings

  # Remove punctuation from tokens for matching
  tokens_clean <- stringr::str_replace_all(tokens, "[[:punct:]]", "")

  if (length(tokens_clean) == 0) {
    return("other")
  }

  # Count how many tokens are backchannel cues
  n_backchannel_matches <- sum(tokens_clean %in% tolower(BACKCHANNEL_CUES))
  match_proportion <- n_backchannel_matches / length(tokens_clean)

  # Python's Rule 1: If EVERY word is a backchannel cue → BACKCHANNEL (regardless of length)
  if (match_proportion == 1.0) {
    return("backchannel")
  }

  # Python's Rule 2: If >word_max AND >duration_max → SECONDARY
  # Note: Python uses backchannel_second_max = 0.0 in natural_turn_wide, so duration check is effectively disabled
  if (n_words > backchannel_word_max && duration > backchannel_second_max) {
    return("secondary")
  }

  # Python's Rule 3: If starts with a NOT_BACKCHANNEL_CUE → SECONDARY
  first_token <- tokens_clean[1]
  if (first_token %in% tolower(NOT_BACKCHANNEL_CUES)) {
    return("secondary")
  }

  # Python's Rule 4 (implicit): Check proportion of backchannel cues
  # If we got here, it's short enough, doesn't start with NOT_BC word
  # Check if enough words are backchannel cues
  if (match_proportion >= backchannel_proportion) {
    return("backchannel")
  }

  # Default: if it's short and doesn't violate rules, consider it backchannel
  # This matches Python's behavior for short turns that pass the filters
  if (n_words <= backchannel_word_max) {
    return("backchannel")
  }

  return("secondary")
}

#' Classify Turns with Overlaps
#'
#' Classifies each turn as primary or secondary/backchannel. A turn is
#' secondary if it is completely contained within another turn (different
#' speaker, starts after and ends before/at the same time as the containing turn).
#'
#' @param df Data frame with collapsed turns (from
#'   \code{\link{collapse_turns_preserving_overlaps}}).
#' @param backchannel_word_max Maximum word count for backchannel classification.
#'   Default: 3.
#' @param backchannel_second_max Maximum duration for backchannel classification.
#'   Default: 0.0.
#' @param backchannel_proportion Minimum proportion of words that must be
#'   backchannel cues. Default: 0.5.
#'
#' @return Data frame with added columns:
#'   \itemize{
#'     \item \code{is_primary}: Logical, whether turn is primary
#'     \item \code{utterance_type}: Character, "primary", "backchannel", or "secondary"
#'   }
#'
#' @details Secondary turns are those that overlap completely within a primary
#'   turn from a different speaker. The function then uses
#'   \code{\link{determine_utterance_type}} to further classify secondary turns
#'   as backchannels or secondary turns.
#'
#' @importFrom dplyr arrange mutate row_number
#' @keywords internal
classify_turns_with_overlaps <- function(df,
                                         backchannel_word_max = 3,
                                         backchannel_second_max = 0.0,
                                         backchannel_proportion = 0.5) {

  df <- df %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(
      row_id = dplyr::row_number(),
      is_primary = TRUE,
      utterance_type = "primary"
    )

  # Check each turn to see if it's contained within any previous turn
  for (i in 1:nrow(df)) {
    turn_start <- df$start[i]
    turn_stop <- df$stop[i]
    turn_speaker <- df$speaker[i]

    # Look at all previous turns
    if (i > 1) {
      for (j in 1:(i-1)) {
        prev_start <- df$start[j]
        prev_stop <- df$stop[j]
        prev_speaker <- df$speaker[j]

        # Check if current turn is CONTAINED within previous turn
        # (different speaker, starts after prev starts, ends before/at prev ends)
        if (prev_speaker != turn_speaker &&
            turn_start >= prev_start &&
            turn_start < prev_stop &&
            turn_stop <= prev_stop) {

          # This turn is contained - it's secondary
          df$is_primary[i] <- FALSE

          # Use Python's algorithm to determine backchannel vs secondary
          df$utterance_type[i] <- determine_utterance_type(
            utterance_text = df$utterance[i],
            n_words = df$n_words[i],
            duration = df$duration[i],
            backchannel_word_max = backchannel_word_max,
            backchannel_second_max = backchannel_second_max,
            backchannel_proportion = backchannel_proportion
          )

          break  # Once we find a containing turn, stop looking
        }
      }
    }
  }

  return(df)
}


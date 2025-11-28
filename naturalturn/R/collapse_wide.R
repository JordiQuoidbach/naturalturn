################################################################################
# Main Collapse Function
# 
# Main function to collapse turns into wide format
################################################################################

#' Process Transcript with NaturalTurn Algorithm
#'
#' Main function implementing the NaturalTurn algorithm. Collapses short pauses
#' within the same speaker, classifies turns as primary/secondary/backchannel,
#' and outputs data in wide format with one row per primary turn and listener
#' overlaps in columns.
#'
#' @param transcript_df Data frame with transcript data. Must contain columns
#'   for speaker, text, start time, and stop time.
#' @param speaker_id_col Name of column containing speaker identifier. Default: "speaker".
#' @param text_col Name of column containing utterance text. Default: "text".
#' @param start_col Name of column containing start time in seconds. Default: "start".
#' @param stop_col Name of column containing stop time in seconds. Default: "stop".
#' @param max_pause Maximum pause (seconds) between consecutive segments from
#'   the same speaker to be merged into one turn. Default: 1.5.
#' @param backchannel_word_max Maximum word count for backchannel classification.
#'   Default: 3.
#' @param backchannel_proportion Minimum proportion of words that must be
#'   backchannel cues. Default: 0.5 (50%).
#' @param interruption_duration_min Minimum duration (seconds) for a turn to be
#'   considered an interruption. Default: 6.0.
#' @param interruption_lag_duration_min Minimum duration (seconds) of the
#'   previous turn for current turn to be an interruption. Default: 1.0.
#'
#' @return Data frame in wide format with one row per primary turn. See
#'   \code{\link{pivot_to_wide_format}} for column descriptions.
#'
#' @details This function implements the NaturalTurn algorithm by Cooney &
#'   Reece (2025). The algorithm:
#'   \itemize{
#'     \item Collapses short pauses (< \code{max_pause}) within the same speaker
#'     \item Classifies turns as primary (main speaker) or secondary/backchannel
#'       (overlapping speech)
#'     \item Joins consecutive primary turns from the same speaker
#'     \item Assigns turn IDs, grouping secondary turns with their primary
#'     \item Pivots to wide format with listener overlaps in columns
#'   }
#'
#'   Interruption detection and within-speech pause tracking are implemented
#'   based on Di Stasi et al. (2024).
#'
#' @references
#'   \itemize{
#'     \item Cooney, G., & Reece, A. (2025). NaturalTurn: A method to segment
#'       speech into psychologically meaningful conversational turns. \emph{Scientific Reports},
#'       15, 39155. \url{https://doi.org/10.1038/s41598-025-24381-1}
#'     \item Di Stasi, M., Templeton, E., & Quoidbach, J. (2024). Zooming out on
#'       bargaining tables: Exploring which conversation dynamics predict negotiation
#'       outcomes. \emph{Journal of Applied Psychology}, 109, 1077-1093.
#'   }
#'
#'   This package is an R implementation of the Python companion repository:
#'   \url{https://github.com/betterup/natural-turn-transcription}
#'
#' @examples
#' \dontrun{
#' # Example transcript data
#' transcript <- data.frame(
#'   speaker = c("A", "A", "B", "A", "B"),
#'   text = c("Hello", "there", "Hi", "How are you", "Good"),
#'   start = c(0.0, 1.2, 1.5, 3.0, 3.5),
#'   stop = c(1.0, 2.0, 2.5, 4.0, 4.2)
#' )
#'
#' # Process transcript with default column names
#' wide_result <- natural_turn_transcript(transcript)
#'
#' # Or specify custom column names
#' wide_result <- natural_turn_transcript(
#'   transcript,
#'   speaker_id_col = "speaker",
#'   text_col = "text",
#'   start_col = "start",
#'   stop_col = "stop"
#' )
#' }
#'
#' @importFrom dplyr arrange mutate lag group_by summarise first select any_of
#' @export
natural_turn_transcript <- function(transcript_df,
                                    speaker_id_col = "speaker",
                                    text_col = "text",
                                    start_col = "start",
                                    stop_col = "stop",
                                    max_pause = 1.5,
                                    backchannel_word_max = 3,
                                    backchannel_proportion = 0.5,
                                    interruption_duration_min = 6.0,
                                    interruption_lag_duration_min = 1.0) {

  # Input validation
  if (!is.data.frame(transcript_df)) {
    stop("transcript_df must be a data frame")
  }
  
  required_cols <- c(speaker_id_col, text_col, start_col, stop_col)
  missing_cols <- setdiff(required_cols, names(transcript_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  if (nrow(transcript_df) == 0) {
    warning("transcript_df is empty, returning empty data frame")
    return(data.frame())
  }

  # Step 1: Collapse short pauses (but preserve overlaps)
  collapsed_df <- collapse_turns_preserving_overlaps(
    transcript_df,
    max_pause = max_pause,
    speaker_id_col = speaker_id_col,
    text_col = text_col,
    start_col = start_col,
    stop_col = stop_col
  )

  # Step 2: Classify turns as primary/secondary/backchannel (Python's algorithm)
  classified_df <- classify_turns_with_overlaps(
    collapsed_df,
    backchannel_word_max = backchannel_word_max,
    backchannel_second_max = 0.0,  # Python's natural_turn_wide uses 0.0
    backchannel_proportion = backchannel_proportion
  )

  # Step 2.5: Join consecutive primary turns from same speaker
  # This mimics Python's _join_contiguous_primary_utterances
  # IMPORTANT: Join ALL consecutive primary turns from same speaker,
  # even if pauses > max_pause (as long as no other speaker interrupted)
  classified_df <- classified_df %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(
      speaker_changed = speaker != dplyr::lag(speaker, default = ""),
      is_contiguous_primary = !speaker_changed & is_primary & dplyr::lag(is_primary, default = FALSE)
    )

  # Mark groups of contiguous same-speaker primary turns
  turn_group_id <- cumsum(!classified_df$is_contiguous_primary)

  # Join contiguous primary turns, TRACKING internal pauses
  final_df <- classified_df %>%
    dplyr::mutate(turn_group_id = turn_group_id) %>%
    dplyr::group_by(turn_group_id) %>%
    dplyr::arrange(start) %>%
    dplyr::summarise(
      speaker = dplyr::first(speaker),
      start = min(start),
      stop = max(stop),
      utterance = paste(utterance, collapse = " "),
      duration = max(stop) - min(start),
      pause = dplyr::first(pause),
      n_words = sum(n_words),
      n_questions = sum(n_questions),
      ends_with_question = any(ends_with_question),
      n_utterances_merged = sum(n_utterances_merged),
      is_primary = dplyr::first(is_primary),
      utterance_type = dplyr::first(utterance_type),
      # TRACK INTERNAL PAUSES (pauses between segments within this turn)
      internal_pauses = if(n() > 1) {
        # Calculate pause between each consecutive segment
        starts <- start
        stops <- stop
        pauses <- starts[-1] - stops[-length(stops)]
        list(pauses)
      } else {
        list(numeric(0))
      },
      n_segments = dplyr::n(),  # How many original segments were merged
      .groups = "drop"
    ) %>%
    dplyr::arrange(start) %>%
    dplyr::select(-turn_group_id)

  # Step 2.6: Count and extract long pauses (>= max_pause threshold)
  final_df <- final_df %>%
    dplyr::mutate(
      # Count pauses meeting or exceeding threshold
      n_long_pauses = sapply(internal_pauses, function(p) sum(p >= max_pause)),

      # Extract long pauses into separate columns
      long_pauses_list = lapply(internal_pauses, function(p) p[p >= max_pause])
    )

  # Create individual pause columns (pause_1, pause_2, etc.)
  max_long_pauses <- max(final_df$n_long_pauses, 0)

  if (max_long_pauses > 0) {
    for (i in 1:max_long_pauses) {
      col_name <- paste0("internal_pause_", i)
      final_df[[col_name]] <- sapply(final_df$long_pauses_list, function(pauses) {
        if (length(pauses) >= i) pauses[i] else NA_real_
      })
    }
  }

  # Clean up temporary columns
  final_df <- final_df %>%
    dplyr::select(-long_pauses_list)

  # Step 3: Assign turn IDs
  grouped_df <- assign_wide_turn_ids(final_df)

  # Step 4: Pivot to wide format
  wide_df <- pivot_to_wide_format(grouped_df,
                                  interruption_duration_min = interruption_duration_min,
                                  interruption_lag_duration_min = interruption_lag_duration_min)

  return(wide_df)
}


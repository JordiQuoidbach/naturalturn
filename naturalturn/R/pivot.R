################################################################################
# Pivot to Wide Format
# 
# Converts long format (one row per turn) to wide format (one row per primary
# turn with listener overlaps in columns)
################################################################################

#' Pivot to Wide Format
#'
#' Converts the long-format turn data to wide format, with one row per primary
#' turn and listener overlaps (backchannels/secondary turns) stored in columns.
#'
#' @param df Data frame with assigned turn IDs (from
#'   \code{\link{assign_wide_turn_ids}}).
#' @param interruption_duration_min Minimum duration (seconds) for a turn to be
#'   considered an interruption. Default: 6.0.
#' @param interruption_lag_duration_min Minimum duration (seconds) of the
#'   previous turn for current turn to be an interruption. Default: 1.0.
#'
#' @return Data frame in wide format with one row per primary turn. Columns include:
#'   \itemize{
#'     \item Primary speaker columns: \code{speaker}, \code{start}, \code{stop},
#'       \code{duration}, \code{utterance}, \code{responseTime}, \code{interruption},
#'       \code{n_words_speaker}, etc.
#'     \item Listener overlap columns: \code{n_listener_turns}, \code{overlap_speaker},
#'       \code{utterance_listener} (concatenated), and \code{*_listener_list} columns
#'       (lists of individual listener turns)
#'   }
#'
#' @details Interruption detection follows criteria from Di Stasi et al. (2024),
#'   based on Farley (2008): a turn is flagged as an interruption if all of the
#'   following are true:
#'   \itemize{
#'     \item Current turn duration > \code{interruption_duration_min}
#'     \item Current turn starts before previous turn ends (pause < 0, overlap)
#'     \item Previous turn duration > \code{interruption_lag_duration_min}
#'     \item Current speaker is different from previous speaker
#'   }
#'
#' @references
#'   \itemize{
#'     \item Di Stasi, M., Templeton, E., & Quoidbach, J. (2024). Zooming out on
#'       bargaining tables: Exploring which conversation dynamics predict negotiation
#'       outcomes. \emph{Journal of Applied Psychology}, 109, 1077-1093.
#'   }
#'
#' @importFrom dplyr filter select mutate lag left_join group_by summarise arrange any_of coalesce first n
#' @keywords internal
pivot_to_wide_format <- function(df,
                                 interruption_duration_min = 6.0,
                                 interruption_lag_duration_min = 1.0) {

  # Identify pause columns
  pause_cols <- names(df)[grepl("^internal_pause_", names(df))]

  # Create primary turns dataframe
  primary_df <- df %>%
    dplyr::filter(is_primary) %>%
    dplyr::select(
      turn_id,
      speaker,
      start,
      stop,
      utterance,
      utterance_type,
      pause,
      duration,
      n_words,
      n_questions,
      ends_with_question,
      n_utterances_merged,
      n_segments,
      n_long_pauses,
      internal_pauses,
      dplyr::any_of(pause_cols)  # Include all internal_pause_* columns
    )

  # Create secondary turns dataframe (if any exist)
  secondary_df <- df %>%
    dplyr::filter(!is_primary)

  if (nrow(secondary_df) > 0) {
    # Store each listener turn separately as list items
    secondary_summary <- secondary_df %>%
      dplyr::group_by(turn_id) %>%
      dplyr::arrange(start) %>%
      dplyr::summarise(
        # Store as LISTS to preserve individual listener turns
        utterance_listener_list = list(utterance),
        utterance_type_listener_list = list(utterance_type),
        start_listener_list = list(start),
        stop_listener_list = list(stop),
        duration_listener_list = list(stop - start),
        pause_listener_list = list(pause),
        n_words_listener_list = list(n_words),
        n_questions_listener_list = list(n_questions),
        ends_with_question_listener_list = list(ends_with_question),

        # Also keep concatenated versions for convenience
        utterance_listener = paste(utterance, collapse = " "),
        n_listener_turns = dplyr::n(),
        .groups = "drop"
      )

    # Join
    wide_df <- primary_df %>%
      dplyr::left_join(secondary_summary, by = "turn_id")
  } else {
    # No secondary turns
    wide_df <- primary_df %>%
      dplyr::mutate(
        utterance_listener_list = list(character(0)),
        utterance_type_listener_list = list(character(0)),
        start_listener_list = list(numeric(0)),
        stop_listener_list = list(numeric(0)),
        duration_listener_list = list(numeric(0)),
        pause_listener_list = list(numeric(0)),
        n_words_listener_list = list(numeric(0)),
        n_questions_listener_list = list(numeric(0)),
        ends_with_question_listener_list = list(logical(0)),
        utterance_listener = NA_character_,
        n_listener_turns = 0L
      )
  }

  # Rename and select columns to match Python format
  # Keep pause tracking columns
  pause_cols_in_df <- names(wide_df)[grepl("^internal_pause_", names(wide_df))]

  wide_df <- wide_df %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(
      duration = stop - start,
      overlap_speaker = !is.na(utterance_listener),
      # Calculate lag duration (previous turn's duration) for interruption detection
      lag_duration = dplyr::lag(duration, default = NA_real_),
      lag_speaker = dplyr::lag(speaker, default = NA_character_)
    ) %>%
    # Flag interruptions based on Farley 2008 criteria
    # Interruption = 1 if: duration > interruption_duration_min AND pause < 0 (overlap)
    # AND lag_duration > interruption_lag_duration_min
    # AND speaker is different from previous speaker (interrupting someone else)
    dplyr::mutate(
      interruption = ifelse(
        duration > interruption_duration_min &
        pause < 0 &
        !is.na(lag_duration) &
        lag_duration > interruption_lag_duration_min &
        !is.na(lag_speaker) &
        speaker != lag_speaker,  # Must be interrupting a different speaker
        1,
        0
      )
    ) %>%
    dplyr::select(
      # Primary speaker columns
      speaker,
      start,
      stop,
      duration,
      utterance,
      responseTime = pause,  # Time gap before this turn (response time)
      interruption,  # Flag: 1 if this turn interrupts the previous turn (Farley 2008)
      n_words_speaker = n_words,
      n_questions_speaker = n_questions,
      ends_with_question_speaker = ends_with_question,
      n_utterances_merged_speaker = n_utterances_merged,

      # Pause tracking columns (primary turn internal structure)
      n_segments,
      n_long_pauses,
      internal_pauses,
      dplyr::any_of(pause_cols_in_df),

      # Listener overlap information
      n_listener_turns,
      overlap_speaker,

      # CONCATENATED listener data (for convenience)
      utterance_listener,

      # LIST columns - Individual listener turns (for detailed analysis)
      utterance_listener_list,
      utterance_type_listener_list,
      start_listener_list,
      stop_listener_list,
      duration_listener_list,
      pause_listener_list,
      n_words_listener_list,
      n_questions_listener_list,
      ends_with_question_listener_list
    ) %>%
    # Remove temporary columns
    dplyr::select(-lag_duration, -lag_speaker)

  return(wide_df)
}


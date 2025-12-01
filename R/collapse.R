################################################################################
# Core Collapsing Function
# 
# Collapses short pauses within the same speaker while preserving overlaps
# between different speakers.
################################################################################

#' Collapse Turns Preserving Overlaps
#'
#' Collapses consecutive speech segments from the same speaker when separated
#' by pauses shorter than \code{max_pause}, while preserving overlaps between
#' different speakers. This is the first step in the NaturalTurn algorithm.
#'
#' @param transcript_df Data frame with transcript data. Must contain columns
#'   for speaker, text, start time, and stop time.
#' @param max_pause Maximum pause (in seconds) between consecutive segments from
#'   the same speaker to be merged into one turn. Default: 1.5 seconds.
#' @param short_pause_threshold Minimum pause duration (in seconds) to be
#'   considered a true pause (vs. stop closures). Default: 0.18 (180ms), based
#'   on Heldner & Edlund (2010).
#' @param speaker_id_col Name of column containing speaker identifier. Default: "speaker".
#' @param text_col Name of column containing utterance text. Default: "text".
#' @param start_col Name of column containing start time in seconds. Default: "start".
#' @param stop_col Name of column containing stop time in seconds. Default: "stop".
#'
#' @return Data frame with collapsed turns, including columns:
#'   \itemize{
#'     \item \code{speaker}: Speaker identifier
#'     \item \code{start}: Start time of turn
#'     \item \code{stop}: Stop time of turn
#'     \item \code{utterance}: Concatenated text from merged segments
#'     \item \code{n_utterances_merged}: Number of original segments merged
#'     \item \code{short_pauses}: List of short pauses (>= 180ms, < max_pause)
#'       within the collapsed turn
#'     \item \code{duration}: Turn duration (stop - start)
#'     \item \code{pause}: Pause before this turn (gap from previous turn)
#'     \item \code{n_words}: Word count
#'     \item \code{n_questions}: Number of question marks
#'     \item \code{ends_with_question}: Logical, whether turn ends with "?"
#'   }
#'
#' @details This function processes each speaker separately, identifying groups
#'   of consecutive segments with pauses < \code{max_pause} and merging them.
#'   Overlaps between different speakers are preserved (not collapsed).
#'
#'   Short pauses (>= \code{short_pause_threshold} but < \code{max_pause}) are
#'   tracked separately from long pauses. The 180ms threshold distinguishes true
#'   pauses from stop closures (brief airflow blockages for consonants), per
#'   Heldner & Edlund (2010).
#'
#'   This is part of the NaturalTurn algorithm implementation. See
#'   \code{\link{natural_turn_transcript}} for the complete algorithm.
#'
#' @references
#'   Heldner, M., & Edlund, J. (2010). Pauses, gaps and overlaps in conversations.
#'   \emph{Journal of Phonetics}, 38(4), 555-568.
#'
#' @examples
#' \dontrun{
#' # Example transcript data
#' transcript <- data.frame(
#'   speaker = c("A", "A", "B", "A"),
#'   text = c("Hello", "there", "Hi", "How are you"),
#'   start = c(0.0, 1.2, 1.5, 3.0),
#'   stop = c(1.0, 2.0, 2.5, 4.0)
#' )
#'
#' # Collapse pauses < 1.5 seconds
#' collapsed <- collapse_turns_preserving_overlaps(transcript, max_pause = 1.5)
#' }
#'
#' @importFrom dplyr arrange mutate lag filter group_by summarise first n bind_rows
#' @importFrom rlang sym
#' @importFrom stringr str_count str_detect fixed
#' @keywords internal
collapse_turns_preserving_overlaps <- function(transcript_df,
                                               max_pause = 1.5,
                                               short_pause_threshold = 0.18,
                                               speaker_id_col = "speaker",
                                               text_col = "text",
                                               start_col = "start",
                                               stop_col = "stop") {

  # Step 1: Sort by start time
  df <- transcript_df %>%
    dplyr::arrange(!!rlang::sym(start_col)) %>%
    dplyr::mutate(
      original_turn_id = dplyr::row_number(),
      pause_before = !!rlang::sym(start_col) - dplyr::lag(!!rlang::sym(stop_col), default = 0)
    )

  # Step 2: Process each speaker separately (collapse short pauses)
  collapsed_list <- list()

  for (spk in unique(df[[speaker_id_col]])) {
    speaker_turns <- df %>%
      dplyr::filter(!!rlang::sym(speaker_id_col) == spk) %>%
      dplyr::arrange(!!rlang::sym(start_col))

    if (nrow(speaker_turns) == 0) next

    # Identify which turns to collapse based on pause
    turn_groups <- vector("integer", nrow(speaker_turns))
    current_group <- 1
    turn_groups[1] <- current_group

    for (i in 2:nrow(speaker_turns)) {
      pause <- speaker_turns[[start_col]][i] - speaker_turns[[stop_col]][i-1]

      if (pause < max_pause) {
        turn_groups[i] <- current_group
      } else {
        current_group <- current_group + 1
        turn_groups[i] <- current_group
      }
    }

    speaker_turns$turn_group <- turn_groups

    # Collapse turns within each group, tracking short pauses
    collapsed_speaker <- speaker_turns %>%
      dplyr::group_by(turn_group) %>%
      dplyr::summarise(
        speaker = dplyr::first(!!rlang::sym(speaker_id_col)),
        start = min(!!rlang::sym(start_col)),
        stop = max(!!rlang::sym(stop_col)),
        utterance = paste(!!rlang::sym(text_col), collapse = " "),
        n_utterances_merged = dplyr::n(),
        # Track SHORT pauses (>= 180ms but < max_pause) within collapsed segments
        short_pauses = if(dplyr::n() > 1) {
          starts <- !!rlang::sym(start_col)
          stops <- !!rlang::sym(stop_col)
          pauses <- starts[-1] - stops[-length(stops)]
          # Filter to only include pauses >= short_pause_threshold
          list(pauses[pauses >= short_pause_threshold])
        } else {
          list(numeric(0))
        },
        .groups = "drop"
      )

    collapsed_list[[spk]] <- collapsed_speaker
  }

  # Step 3: Combine all speakers and re-sort by time
  # BUT DON'T join consecutive turns from same speaker yet!
  final_df <- dplyr::bind_rows(collapsed_list) %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(
      duration = stop - start,
      pause = start - dplyr::lag(stop, default = 0),
      n_words = stringr::str_count(utterance, "\\S+"),
      n_questions = stringr::str_count(utterance, stringr::fixed("?")),
      ends_with_question = stringr::str_detect(utterance, "\\?\\s*$")
    )

  return(final_df)
}


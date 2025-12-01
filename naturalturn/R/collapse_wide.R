################################################################################
# Main Collapse Function
# 
# Main function to collapse turns into wide format
################################################################################

#' Process Baseline Transcript with NaturalTurn Algorithm
#'
#' Takes a baseline transcript and applies the NaturalTurn algorithm. Collapses
#' short pauses within the same speaker, classifies turns as primary/secondary/
#' backchannel, and outputs data in either wide or long format.
#'
#' @param baseline_df Data frame with baseline transcript data. Must contain
#'   columns for speaker, text, start time, and stop time. Typically created
#'   by \code{\link{json_to_baseline}}.
#' @param speaker_id_col Name of column containing speaker identifier. Default: "speaker".
#' @param text_col Name of column containing utterance text. Default: "text".
#' @param start_col Name of column containing start time in seconds. Default: "start".
#' @param stop_col Name of column containing stop time in seconds. Default: "stop".
#' @param max_pause Maximum pause (seconds) between consecutive segments from
#'   the same speaker to be merged into one turn. Default: 1.5.
#' @param short_pause_threshold Minimum pause duration (seconds) to be considered
#'   a true pause vs. stop closures. Default: 0.18 (180ms), based on Heldner &
#'   Edlund (2010). Pauses >= this threshold but < max_pause are "short pauses".
#' @param backchannel_word_max Maximum word count for backchannel classification.
#'   Default: 3.
#' @param backchannel_proportion Minimum proportion of words that must be
#'   backchannel cues. Default: 0.5 (50%).
#' @param interruption_duration_min Minimum duration (seconds) for a turn to be
#'   considered an interruption. Default: 6.0.
#' @param interruption_lag_duration_min Minimum duration (seconds) of the
#'   previous turn for current turn to be an interruption. Default: 1.0.
#' @param output_format Character string specifying output format. Either
#'   \code{"wide"} (default) for one row per primary turn with listener overlaps
#'   in columns, or \code{"long"} for one row per turn (both primary and
#'   secondary/backchannel turns).
#' @param output_csv Optional path to save results as CSV file. If provided,
#'   list columns are automatically converted to JSON-style strings for CSV
#'   compatibility. If \code{NULL} (default), results are only returned
#'   (not saved to disk).
#'
#' @return Data frame in the specified format:
#'   \itemize{
#'     \item \strong{Wide format} (\code{output_format = "wide"}): One row per
#'       primary turn with listener overlaps in columns. See
#'       \code{\link{pivot_to_wide_format}} for column descriptions.
#'     \item \strong{Long format} (\code{output_format = "long"}): One row per
#'       turn with columns: \code{speaker}, \code{start}, \code{stop},
#'       \code{utterance}, \code{duration}, \code{response_time}, \code{n_words},
#'       \code{n_questions}, \code{ends_with_question}, \code{n_utterances_merged},
#'       \code{is_primary}, \code{utterance_type} (primary/secondary/backchannel),
#'       \code{turn_id}, \code{n_segments}, \code{n_short_pauses}, \code{n_long_pauses},
#'       \code{short_pauses}, \code{long_pauses}.
#'   }
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
#' # From JSON files
#' baseline <- json_to_baseline(speaker1_json, speaker2_json)
#' result <- baseline_to_naturalturn(baseline, output_csv = "result.csv")
#'
#' # Process in long format (one row per turn)
#' result_long <- baseline_to_naturalturn(baseline, output_format = "long")
#'
#' # From existing transcript data
#' transcript <- data.frame(
#'   speaker = c("A", "A", "B", "A", "B"),
#'   text = c("Hello", "there", "Hi", "How are you", "Good"),
#'   start = c(0.0, 1.2, 1.5, 3.0, 3.5),
#'   stop = c(1.0, 2.0, 2.5, 4.0, 4.2)
#' )
#' result <- baseline_to_naturalturn(transcript)
#' }
#'
#' @seealso \code{\link{json_to_baseline}}, \code{\link{baseline_to_naturalturn_batch}}
#'
#' @importFrom dplyr arrange mutate lag group_by summarise first select any_of
#' @export
baseline_to_naturalturn <- function(baseline_df,
                                    speaker_id_col = "speaker",
                                    text_col = "text",
                                    start_col = "start",
                                    stop_col = "stop",
                                    max_pause = 1.5,
                                    short_pause_threshold = 0.18,
                                    backchannel_word_max = 3,
                                    backchannel_proportion = 0.5,
                                    interruption_duration_min = 6.0,
                                    interruption_lag_duration_min = 1.0,
                                    output_format = c("wide", "long"),
                                    output_csv = NULL) {

  # Input validation
  if (!is.data.frame(baseline_df)) {
    stop("baseline_df must be a data frame")
  }
  
  output_format <- match.arg(output_format)
  
  required_cols <- c(speaker_id_col, text_col, start_col, stop_col)
  missing_cols <- setdiff(required_cols, names(baseline_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  if (nrow(baseline_df) == 0) {
    warning("baseline_df is empty, returning empty data frame")
    return(data.frame())
  }

  # Step 1: Collapse short pauses (but preserve overlaps)
  # Short pauses (>= 180ms but < max_pause) are tracked within collapsed segments
  collapsed_df <- collapse_turns_preserving_overlaps(
    baseline_df,
    max_pause = max_pause,
    short_pause_threshold = short_pause_threshold,
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
  # Short pauses (>= 180ms, < max_pause) come from collapsed segments
  # Long pauses (>= max_pause) come from joining segments here
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
      # Combine SHORT pauses from all collapsed segments
      short_pauses = list(unlist(short_pauses)),
      # TRACK LONG PAUSES (pauses >= max_pause between segments joined here)
      long_pauses = if(dplyr::n() > 1) {
        # Calculate pause between each consecutive segment
        starts <- start
        stops <- stop
        pauses <- starts[-1] - stops[-length(stops)]
        list(pauses)  # These are all >= max_pause by definition
      } else {
        list(numeric(0))
      },
      n_segments = dplyr::n(),  # How many segments were joined here
      .groups = "drop"
    ) %>%
    dplyr::arrange(start) %>%
    dplyr::select(-turn_group_id)

  # Step 2.6: Count short and long pauses
  final_df <- final_df %>%
    dplyr::mutate(
      n_short_pauses = sapply(short_pauses, length),
      n_long_pauses = sapply(long_pauses, length)
    )

  # Create individual long pause columns (long_pause_1, long_pause_2, etc.)
  max_long_pauses <- max(final_df$n_long_pauses, 0)

  if (max_long_pauses > 0) {
    for (i in 1:max_long_pauses) {
      col_name <- paste0("long_pause_", i)
      final_df[[col_name]] <- sapply(final_df$long_pauses, function(pauses) {
        if (length(pauses) >= i) pauses[i] else NA_real_
      })
    }
  }

  # Create individual short pause columns (short_pause_1, short_pause_2, etc.)
  max_short_pauses <- max(final_df$n_short_pauses, 0)

  if (max_short_pauses > 0) {
    for (i in 1:max_short_pauses) {
      col_name <- paste0("short_pause_", i)
      final_df[[col_name]] <- sapply(final_df$short_pauses, function(pauses) {
        if (length(pauses) >= i) pauses[i] else NA_real_
      })
    }
  }

  # Step 3: Assign turn IDs
  grouped_df <- assign_wide_turn_ids(final_df)

  # Step 4: Return in requested format

  if (output_format == "long") {
    # Long format: one row per turn (both primary and secondary)
    # Clean up and arrange output columns
    long_df <- grouped_df %>%
      dplyr::arrange(start) %>%
      dplyr::select(
        turn_id,
        speaker,
        start,
        stop,
        duration,
        utterance,
        utterance_type,
        is_primary,
        response_time = pause,  # Time gap before this turn
        n_words,
        n_questions,
        ends_with_question,
        n_utterances_merged,
        n_segments,
        n_short_pauses,
        n_long_pauses,
        short_pauses,
        long_pauses,
        dplyr::any_of(names(grouped_df)[grepl("^short_pause_", names(grouped_df))]),
        dplyr::any_of(names(grouped_df)[grepl("^long_pause_", names(grouped_df))])
      )
    
    # Save to CSV if output_csv is provided
    if (!is.null(output_csv)) {
      save_naturalturn_csv(long_df, output_csv, output_format = "long")
    }
    
    return(long_df)
  }

  # Wide format: one row per primary turn with listener overlaps in columns
  wide_df <- pivot_to_wide_format(grouped_df,
                                  interruption_duration_min = interruption_duration_min,
                                  interruption_lag_duration_min = interruption_lag_duration_min)

  # Save to CSV if output_csv is provided
  if (!is.null(output_csv)) {
    save_naturalturn_csv(wide_df, output_csv, output_format = "wide")
  }

  return(wide_df)
}


#' Save NaturalTurn Results to CSV
#'
#' Helper function to save NaturalTurn results to CSV, converting list columns
#' to JSON-style strings for compatibility.
#'
#' @param df Data frame with NaturalTurn results.
#' @param output_csv Path to save CSV file.
#' @param output_format Either "wide" or "long" to determine which list columns to convert.
#'
#' @return Invisibly returns the path to the saved CSV file.
#'
#' @importFrom readr write_csv
#' @keywords internal
save_naturalturn_csv <- function(df, output_csv, output_format = "wide") {
  
  # Helper to convert list to JSON-style string
  list_to_json_string <- function(x, quote_values = FALSE) {
    sapply(x, function(lst) {
      if (length(lst) == 0) return(NA_character_)
      if (quote_values) {
        paste0("[", paste(sapply(lst, function(v) paste0('"', v, '"')), collapse = ", "), "]")
      } else {
        paste0("[", paste(lst, collapse = ", "), "]")
      }
    })
  }
  
  # Helper for boolean lists
  list_to_json_bool <- function(x) {
    sapply(x, function(lst) {
      if (length(lst) == 0) return(NA_character_)
      paste0("[", paste(tolower(as.character(lst)), collapse = ", "), "]")
    })
  }
  
  # Convert list columns based on output format
  df_for_csv <- df
  
  if (output_format == "wide") {
    # Wide format list columns
    if ("utterance_listener_list" %in% names(df_for_csv)) {
      df_for_csv$utterance_listener_list <- list_to_json_string(df_for_csv$utterance_listener_list, quote_values = TRUE)
    }
    if ("utterance_type_listener_list" %in% names(df_for_csv)) {
      df_for_csv$utterance_type_listener_list <- list_to_json_string(df_for_csv$utterance_type_listener_list, quote_values = TRUE)
    }
    if ("start_listener_list" %in% names(df_for_csv)) {
      df_for_csv$start_listener_list <- list_to_json_string(df_for_csv$start_listener_list)
    }
    if ("stop_listener_list" %in% names(df_for_csv)) {
      df_for_csv$stop_listener_list <- list_to_json_string(df_for_csv$stop_listener_list)
    }
    if ("duration_listener_list" %in% names(df_for_csv)) {
      df_for_csv$duration_listener_list <- list_to_json_string(df_for_csv$duration_listener_list)
    }
    if ("pause_listener_list" %in% names(df_for_csv)) {
      df_for_csv$pause_listener_list <- list_to_json_string(df_for_csv$pause_listener_list)
    }
    if ("n_words_listener_list" %in% names(df_for_csv)) {
      df_for_csv$n_words_listener_list <- list_to_json_string(df_for_csv$n_words_listener_list)
    }
    if ("n_questions_listener_list" %in% names(df_for_csv)) {
      df_for_csv$n_questions_listener_list <- list_to_json_string(df_for_csv$n_questions_listener_list)
    }
    if ("ends_with_question_listener_list" %in% names(df_for_csv)) {
      df_for_csv$ends_with_question_listener_list <- list_to_json_bool(df_for_csv$ends_with_question_listener_list)
    }
  }
  
  # Common list columns (both formats)
  if ("short_pauses" %in% names(df_for_csv)) {
    df_for_csv$short_pauses <- sapply(df_for_csv$short_pauses, function(x) {
      if (length(x) == 0) return("[]")
      paste0("[", paste(x, collapse = ", "), "]")
    })
  }
  if ("long_pauses" %in% names(df_for_csv)) {
    df_for_csv$long_pauses <- sapply(df_for_csv$long_pauses, function(x) {
      if (length(x) == 0) return("[]")
      paste0("[", paste(x, collapse = ", "), "]")
    })
  }
  
  # Write CSV
  readr::write_csv(df_for_csv, output_csv)
  
  # Also save RDS for native R usage
  rds_path <- sub("\\.csv$", ".rds", output_csv)
  saveRDS(df, rds_path)
  
  message(sprintf("Saved CSV: %s", output_csv))
  message(sprintf("Saved RDS: %s (use readRDS() to preserve list columns)", rds_path))
  
  invisible(output_csv)
}


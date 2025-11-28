################################################################################
# Batch Processing Function
# 
# Process multiple conversations from a data frame
################################################################################

#' Process Batch of Conversations with NaturalTurn Algorithm
#'
#' Processes multiple conversations from a data frame, applying the NaturalTurn
#' algorithm to each conversation separately and combining results.
#'
#' @param transcripts_df Data frame containing transcript data for multiple
#'   conversations. Must contain a column to identify different conversations
#'   (e.g., \code{conversation_id}).
#' @param output_csv Optional path to save results as CSV file. If \code{NULL},
#'   results are only returned (not saved to disk). Default: \code{NULL}.
#' @param max_pause Maximum pause (seconds) between consecutive segments from
#'   the same speaker to be merged. Default: 1.5.
#' @param backchannel_word_max Maximum word count for backchannel classification.
#'   Default: 3.
#' @param backchannel_proportion Minimum proportion of words that must be
#'   backchannel cues. Default: 0.5.
#' @param interruption_duration_min Minimum duration (seconds) for a turn to be
#'   considered an interruption. Default: 6.0.
#' @param interruption_lag_duration_min Minimum duration (seconds) of the
#'   previous turn for current turn to be an interruption. Default: 1.0.
#' @param conversation_id_col Name of column containing conversation identifier.
#'   Default: auto-detects "conversation_id" or "nego_id".
#' @param speaker_col Name of column containing speaker identifier.
#'   Default: auto-detects "email", "speaker", or "role".
#' @param text_col Name of column containing utterance text. Default: "text".
#' @param start_col Name of column containing start time in seconds. Default: "time.s".
#' @param stop_col Name of column containing stop time in seconds. Default: "time.e".
#'
#' @return Data frame with processed turns for all conversations. Also saves
#'   results to CSV (and RDS) if \code{output_csv} is provided.
#'
#' @details This function:
#'   \itemize{
#'     \item Groups by \code{conversation_id_col} and processes each conversation separately
#'     \item Automatically detects speaker column (\code{email} preferred over \code{speaker} or \code{role})
#'     \item Preserves all metadata columns from the original data
#'     \item Combines results and optionally saves to CSV and RDS formats
#'   }
#'
#'   The RDS format preserves list columns (e.g., \code{utterance_listener_list})
#'   in their native R format, while CSV converts them to JSON-style strings.
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
#' @section Parameter Guide:
#' \describe{
#'   \item{\code{max_pause}}{Maximum pause between consecutive speech segments
#'     from the SAME speaker to be merged into one turn. Pauses > max_pause
#'     WITHIN a merged turn are tracked in \code{internal_pause_*} columns.}
#'   \item{\code{backchannel_word_max}}{Maximum word count for an overlapping
#'     utterance to be considered a "backchannel" (vs "secondary").}
#'   \item{\code{backchannel_proportion}}{Minimum proportion of words that must
#'     be backchannel cues for a short utterance to be classified as backchannel.}
#'   \item{\code{interruption_duration_min}}{Minimum duration for a turn to be
#'     considered an interruption (Farley 2008 criteria).}
#'   \item{\code{interruption_lag_duration_min}}{Minimum duration of the previous
#'     turn for current turn to be an interruption.}
#' }
#'
#' @examples
#' \dontrun{
#' # Load your transcripts data frame
#' transcripts <- read.csv("transcripts.csv")
#'
#' # Process all conversations
#' result <- natural_turn_batch(
#'   transcripts,
#'   conversation_id_col = "conversation_id",
#'   speaker_col = "speaker",
#'   text_col = "text",
#'   start_col = "start_time",
#'   stop_col = "end_time"
#' )
#'
#' # Optionally save to CSV
#' result <- natural_turn_batch(
#'   transcripts,
#'   output_csv = "processed_transcripts.csv",
#'   conversation_id_col = "conversation_id"
#' )
#' }
#'
#' @importFrom dplyr filter pull mutate row_number select distinct left_join coalesce bind_rows first
#' @importFrom readr write_csv
#' @importFrom rlang sym
#' @export
natural_turn_batch <- function(transcripts_df,
                               output_csv = NULL,
                               max_pause = 1.5,
                               backchannel_word_max = 3,
                               backchannel_proportion = 0.5,
                               interruption_duration_min = 6.0,
                               interruption_lag_duration_min = 1.0,
                               conversation_id_col = NULL,
                               speaker_col = NULL,
                               text_col = "text",
                               start_col = "time.s",
                               stop_col = "time.e") {

  # Input validation
 if (!is.data.frame(transcripts_df)) {
    stop("transcripts_df must be a data frame")
  }
  
  df <- transcripts_df

  # Auto-detect conversation_id column if not specified
  if (is.null(conversation_id_col)) {
    if ("conversation_id" %in% names(df)) {
      conv_id_col <- "conversation_id"
    } else if ("nego_id" %in% names(df)) {
      conv_id_col <- "nego_id"
    } else {
      stop("Could not auto-detect conversation ID column. Please specify 'conversation_id_col'.\nAvailable columns: ", 
           paste(names(df), collapse = ", "))
    }
  } else {
    conv_id_col <- conversation_id_col
  }
  
  if (!conv_id_col %in% names(df)) {
    stop(sprintf("Column '%s' not found. Available columns: %s", 
                 conv_id_col, paste(names(df), collapse = ", ")))
  }
  
  # Auto-detect speaker column if not specified
  if (is.null(speaker_col)) {
    if ("email" %in% names(df)) {
      speaker_col_auto <- "email"
    } else if ("speaker" %in% names(df)) {
      speaker_col_auto <- "speaker"
    } else if ("role" %in% names(df)) {
      speaker_col_auto <- "role"
    } else {
      stop("Could not auto-detect speaker column. Please specify 'speaker_col'.\nAvailable columns: ", 
           paste(names(df), collapse = ", "))
    }
  } else {
    speaker_col_auto <- speaker_col
  }
  
  # Validate required columns
  required_cols <- c(conv_id_col, speaker_col_auto, text_col, start_col, stop_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s\nAvailable columns: %s", 
                 paste(missing_cols, collapse = ", "),
                 paste(names(df), collapse = ", ")))
  }

  conv_ids <- df %>%
    dplyr::filter(!is.na(!!rlang::sym(conv_id_col))) %>%
    dplyr::pull(!!rlang::sym(conv_id_col)) %>%
    unique()

  cat(sprintf("Processing %d conversations...\n", length(conv_ids)))

  all_results <- list()

  for (i in seq_along(conv_ids)) {
    conv_id <- conv_ids[i]

    if (i <= 5 || i %% 50 == 0) {
      cat(sprintf("Processing %d/%d: %s\n", i, length(conv_ids), conv_id))
    }

    conv_data <- df %>%
      dplyr::filter(!!rlang::sym(conv_id_col) == !!conv_id) %>%
      dplyr::filter(!is.na(!!rlang::sym(text_col)), 
                    !is.na(!!rlang::sym(start_col)), 
                    !is.na(!!rlang::sym(stop_col)))

    if (nrow(conv_data) == 0) next

    tryCatch({
      original_turns <- nrow(conv_data)

      wide_result <- natural_turn_transcript(
        conv_data,
        max_pause = max_pause,
        backchannel_word_max = backchannel_word_max,
        backchannel_proportion = backchannel_proportion,
        interruption_duration_min = interruption_duration_min,
        interruption_lag_duration_min = interruption_lag_duration_min,
        speaker_col = speaker_col_auto,
        text_col = text_col,
        start_col = start_col,
        stop_col = stop_col
      )

      primary_turns <- nrow(wide_result)
      listener_turns <- sum(!is.na(wide_result$utterance_listener))

      # Add turn counter
      wide_result <- wide_result %>%
        dplyr::mutate(turn_id = dplyr::row_number())

      # Add ALL metadata columns from original data (constant per conversation)
      # Exclude the columns we've already processed
      exclude_cols <- c(text_col, start_col, stop_col, speaker_col_auto, "speech_turn")
      metadata_cols <- setdiff(names(conv_data), exclude_cols)

      # Add each metadata column (take first value since constant per conversation)
      for (col in metadata_cols) {
        wide_result[[col]] <- dplyr::first(conv_data[[col]])
      }

      # If email was used as speaker, also keep role as metadata
      if (speaker_col_auto == "email" && "role" %in% names(conv_data)) {
        # Map each speaker (email) back to their role
        # Create email-to-role mapping
        email_role_map <- conv_data %>%
          dplyr::select(email, role) %>%
          dplyr::distinct()

        # Join to get role for each turn's speaker
        wide_result <- wide_result %>%
          dplyr::left_join(email_role_map, by = c("speaker" = "email"), suffix = c("", "_from_map"))

        # If there's a role column conflict, keep the mapped one
        if ("role_from_map" %in% names(wide_result)) {
          wide_result <- wide_result %>%
            dplyr::mutate(role = dplyr::coalesce(role_from_map, role)) %>%
            dplyr::select(-role_from_map)
        }
      }

      if (i <= 5 || i %% 50 == 0) {
        cat(sprintf("  %d/%d: %d turns -> %d primary turns (%d with overlap)\n",
                    i, length(conv_ids), original_turns, primary_turns, listener_turns))
      }

      all_results[[as.character(conv_id)]] <- wide_result

    }, error = function(e) {
      cat(sprintf("  Error processing %s: %s\n", conv_id, e$message))
    })
  }

  # Combine results
  final_df <- dplyr::bind_rows(all_results)

  cat(sprintf("\nCompleted: %d conversations, %s primary turns (%.1f%% with overlap)\n",
              length(all_results),
              format(nrow(final_df), big.mark = ","),
              sum(!is.na(final_df$utterance_listener)) / nrow(final_df) * 100))

  # Save results
  if (!is.null(output_csv)) {
    cat(sprintf("Saving to %s...\n", output_csv))

    # Convert list columns to JSON-style strings for CSV compatibility
    final_df_for_csv <- final_df %>%
      dplyr::mutate(
        # Convert numeric/character lists to [val1, val2, ...] format
        utterance_listener_list = sapply(utterance_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(sapply(x, function(v) paste0('"', v, '"')), collapse = ", "), "]")
        }),
        utterance_type_listener_list = sapply(utterance_type_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(sapply(x, function(v) paste0('"', v, '"')), collapse = ", "), "]")
        }),
        start_listener_list = sapply(start_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(x, collapse = ", "), "]")
        }),
        stop_listener_list = sapply(stop_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(x, collapse = ", "), "]")
        }),
        duration_listener_list = sapply(duration_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(x, collapse = ", "), "]")
        }),
        pause_listener_list = sapply(pause_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(x, collapse = ", "), "]")
        }),
        n_words_listener_list = sapply(n_words_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(x, collapse = ", "), "]")
        }),
        n_questions_listener_list = sapply(n_questions_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(x, collapse = ", "), "]")
        }),
        ends_with_question_listener_list = sapply(ends_with_question_listener_list, function(x) {
          if (length(x) == 0) return(NA_character_)
          paste0("[", paste(tolower(x), collapse = ", "), "]")
        }),
        internal_pauses = sapply(internal_pauses, function(x) {
          if (length(x) == 0) return("[]")
          paste0("[", paste(x, collapse = ", "), "]")
        })
      )

    readr::write_csv(final_df_for_csv, output_csv)

    # Also save as RDS for native R usage (preserves true list structure)
    rds_path <- sub("\\.csv$", ".rds", output_csv)
    saveRDS(final_df, rds_path)
    cat(sprintf("Also saved R native format: %s (use readRDS() to preserve list columns)\n", rds_path))
  }

  return(final_df)
}

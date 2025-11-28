################################################################################
# Turn ID Assignment
# 
# Assigns turn IDs, grouping secondary/backchannel turns with their primary
################################################################################

#' Assign Wide Turn IDs
#'
#' Assigns turn IDs to primary and secondary turns, grouping secondary/backchannel
#' turns with the primary turn they overlap with.
#'
#' @param df Data frame with classified turns (from
#'   \code{\link{classify_turns_with_overlaps}}).
#'
#' @return Data frame with added \code{turn_id} column. Primary turns get
#'   sequential IDs starting from 1. Secondary/backchannel turns get the ID
#'   of the primary turn they overlap with.
#'
#' @details Primary turns are assigned sequential IDs. Secondary/backchannel
#'   turns are assigned the ID of the overlapping primary turn (different speaker).
#'   If no overlapping primary is found, they are assigned to the previous primary.
#'
#' @importFrom dplyr arrange filter slice
#' @keywords internal
assign_wide_turn_ids <- function(df) {
  df <- df %>%
    dplyr::arrange(start)

  # Mark primary turns
  df$is_primary <- df$utterance_type == "primary"
  df$turn_id <- NA_integer_

  current_turn_id <- 1
  current_primary_speaker <- ""
  current_primary_start <- -Inf
  current_primary_stop <- -Inf

  for (i in 1:nrow(df)) {
    if (df$is_primary[i]) {
      # This is a primary turn
      df$turn_id[i] <- current_turn_id
      current_primary_speaker <- df$speaker[i]
      current_primary_start <- df$start[i]
      current_primary_stop <- df$stop[i]
      current_turn_id <- current_turn_id + 1
    } else {
      # This is a secondary/backchannel turn
      # Assign it to the primary turn it overlaps with
      turn_start <- df$start[i]
      turn_stop <- df$stop[i]
      turn_speaker <- df$speaker[i]

      # Find overlapping primary turn (different speaker)
      overlapping_primary <- df %>%
        dplyr::filter(
          is_primary,
          speaker != turn_speaker,
          start <= turn_stop,
          stop >= turn_start
        ) %>%
        dplyr::arrange(start) %>%
        dplyr::slice(1)

      if (nrow(overlapping_primary) > 0) {
        df$turn_id[i] <- overlapping_primary$turn_id[1]
      } else {
        # No overlapping primary found - assign to previous primary
        prev_primary_ids <- df$turn_id[1:(i-1)]
        prev_primary_ids <- prev_primary_ids[!is.na(prev_primary_ids)]
        if (length(prev_primary_ids) > 0) {
          df$turn_id[i] <- max(prev_primary_ids)
        }
      }
    }
  }

  return(df)
}


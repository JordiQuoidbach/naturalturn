################################################################################
# Speech-to-Text Provider Integration Functions
# 
# Functions to process transcription outputs from various speech-to-text
# providers (Deepgram, AWS Transcribe, AssemblyAI) and prepare them
# for the NaturalTurn algorithm
################################################################################

#' Combine Two Deepgram Transcriptions into Baseline Transcript
#'
#' Takes two Deepgram API transcription outputs (parsed JSON responses from
#' separate audio files, one per speaker) and combines them into a single
#' data frame ready for processing with \code{\link{natural_turn_batch}} or
#' \code{\link{natural_turn_transcript}}.
#'
#' @param transcript_speaker1 List. The parsed JSON response from Deepgram API
#'   for speaker 1 (as returned by \code{httr::content(response, "parsed")}).
#' @param transcript_speaker2 List. The parsed JSON response from Deepgram API
#'   for speaker 2 (as returned by \code{httr::content(response, "parsed")}).
#' @param speaker1_id Character. Identifier for speaker 1 (e.g., email address,
#'   name, or role). Default: "speaker1".
#' @param speaker2_id Character. Identifier for speaker 2. Default: "speaker2".
#' @param conversation_id Optional character. Identifier for the conversation.
#'   If provided, adds a \code{conversation_id} column to the output. Useful
#'   when processing multiple conversations for batch processing.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{speaker}{Character. The speaker identifier.}
#'   \item{start}{Numeric. Start time of the word in seconds.}
#'   \item{stop}{Numeric. End time of the word in seconds.}
#'   \item{text}{Character. The word with punctuation.}
#'   \item{word}{Character. The word without punctuation.}
#'   \item{confidence}{Numeric. Deepgram's confidence score for the word.}
#'   \item{conversation_id}{Character. Conversation identifier (if provided).}
#' }
#'
#' The data frame is sorted by start time and is ready to be used as input
#' for \code{\link{natural_turn_batch}} or \code{\link{natural_turn_transcript}}.
#'
#' @details
#' This function is designed to work with Deepgram's Nova-2 transcription API
#' or similar. It expects the standard Deepgram response structure where words
#' are nested in \code{results$channels[[1]]$alternatives[[1]]$words}.
#'
#' Each word object in the Deepgram response typically contains:
#' \itemize{
#'   \item \code{word}: The recognized word (lowercase, no punctuation)
#'   \item \code{start}: Start timestamp in seconds
#'   \item \code{end}: End timestamp in seconds
#'   \item \code{punctuated_word}: The word with punctuation
#'   \item \code{confidence}: Recognition confidence (0-1)
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming you have transcription responses from Deepgram API
#' # (see package README for full workflow with API calls)
#'
#' # Combine two speaker transcriptions (uses default speaker1/speaker2 IDs)
#' baseline <- combine_deepgram_transcripts(
#'   transcript_speaker1 = speaker1_response,
#'   transcript_speaker2 = speaker2_response
#' )
#'
#' # Or provide custom speaker IDs
#' baseline <- combine_deepgram_transcripts(
#'   transcript_speaker1 = speaker1_response,
#'   transcript_speaker2 = speaker2_response,
#'   speaker1_id = "participant_A",
#'   speaker2_id = "participant_B",
#'   conversation_id = "session_001"
#' )
#'
#' # Process with NaturalTurn
#' result <- natural_turn_transcript(
#'   baseline,
#'   speaker_id_col = "speaker",
#'   text_col = "text",
#'   start_col = "start",
#'   stop_col = "stop"
#' )
#' }
#'
#' @seealso \code{\link{combine_transcripts}}, \code{\link{natural_turn_transcript}}
#'
#' @importFrom dplyr bind_rows arrange select any_of everything
#' @importFrom rlang .data
#' @keywords internal
combine_deepgram_transcripts <- function(transcript_speaker1,
                                         transcript_speaker2,
                                         speaker1_id = "speaker1",
                                         speaker2_id = "speaker2",
                                         conversation_id = NULL) {
  
  # Input validation
  if (!is.list(transcript_speaker1)) {
    stop("transcript_speaker1 must be a list (parsed Deepgram JSON response)")
  }
  if (!is.list(transcript_speaker2)) {
    stop("transcript_speaker2 must be a list (parsed Deepgram JSON response)")
  }
  
  # Extract words from each transcript
  words1 <- extract_deepgram_words(transcript_speaker1)
  words2 <- extract_deepgram_words(transcript_speaker2)
  
  # Check if both have words
  if (nrow(words1) == 0 && nrow(words2) == 0) {
    warning("Both transcripts are empty. Returning empty data frame.")
    return(data.frame(
      speaker = character(0),
      start = numeric(0),
      stop = numeric(0),
      text = character(0),
      word = character(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Add speaker identifiers
  if (nrow(words1) > 0) {
    words1$speaker <- speaker1_id
  }
  if (nrow(words2) > 0) {
    words2$speaker <- speaker2_id
  }
  
  # Combine and sort by start time
  combined <- dplyr::bind_rows(words1, words2)
  combined <- dplyr::arrange(combined, .data$start)
  
  # Reorder columns to put speaker first
  combined <- dplyr::select(combined, 
                            "speaker", "start", "stop", "text", 
                            dplyr::any_of(c("word", "confidence")))
  
  # Add conversation_id if provided
  if (!is.null(conversation_id)) {
    combined$conversation_id <- conversation_id
    combined <- dplyr::select(combined, 
                              "conversation_id", dplyr::everything())
  }
  
  return(combined)
}


#' Extract Words from Deepgram Transcription Response
#'
#' Extracts the words data frame from a Deepgram API transcription response.
#' This is a helper function used by \code{\link{combine_deepgram_transcripts}}.
#'
#' @param deepgram_response List. The parsed JSON response from Deepgram API
#'   (as returned by \code{httr::content(response, "parsed")} or
#'   \code{jsonlite::fromJSON()}).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{start}{Numeric. Start time of the word in seconds.}
#'   \item{stop}{Numeric. End time of the word in seconds.}
#'   \item{text}{Character. The word with punctuation (from \code{punctuated_word}).}
#'   \item{word}{Character. The word without punctuation.}
#'   \item{confidence}{Numeric. Deepgram's confidence score for the word.}
#' }
#'
#' @details
#' This function navigates the Deepgram response structure. It handles both
#' the structure from \code{httr::content()} (nested lists) and from
#' \code{jsonlite::fromJSON()} (data frames with nested lists).
#'
#' If the expected structure is not found, it returns an empty data frame
#' with appropriate column types.
#'
#' @examples
#' \dontrun{
#' # Extract words from a single speaker's transcription
#' words_df <- extract_deepgram_words(speaker1_response)
#' head(words_df)
#' }
#'
#' @seealso \code{\link{combine_transcripts}}
#'
#' @keywords internal
extract_deepgram_words <- function(deepgram_response) {
  
  # Navigate to words - handle both httr and jsonlite structures
  words_data <- tryCatch({
    channels <- deepgram_response$results$channels
    
    # Check if channels is a data frame (jsonlite) or list (httr)
    if (is.data.frame(channels)) {
      # jsonlite structure: channels is a data frame
      # alternatives is a list column, words is nested inside
      alts <- channels$alternatives[[1]]
      if (is.data.frame(alts)) {
        # words might be a list column in the data frame
        words <- alts$words
        if (is.list(words) && !is.data.frame(words)) {
          words <- words[[1]]  # Get first element if it's a list wrapper
        }
        words
      } else if (is.list(alts)) {
        alts[[1]]$words
      } else {
        NULL
      }
    } else if (is.list(channels)) {
      # httr structure: nested lists
      channels[[1]]$alternatives[[1]]$words
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
  
  # Return empty data frame if no words found
  if (is.null(words_data) || length(words_data) == 0) {
    return(data.frame(
      start = numeric(0),
      stop = numeric(0),
      text = character(0),
      word = character(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Handle data frame format (from jsonlite with simplifyVector)
  if (is.data.frame(words_data)) {
    words_df <- data.frame(
      start = as.numeric(words_data$start),
      stop = as.numeric(words_data$end),
      text = as.character(words_data$punctuated_word %||% words_data$word),
      word = as.character(words_data$word),
      confidence = as.numeric(words_data$confidence),
      stringsAsFactors = FALSE
    )
    return(words_df)
  }
  
  # Handle list format (from httr or jsonlite with simplifyVector=FALSE)
  words_df <- do.call(rbind, lapply(words_data, function(w) {
    data.frame(
      start = as.numeric(w$start %||% NA),
      stop = as.numeric(w$end %||% NA),
      text = as.character(w$punctuated_word %||% w$word %||% ""),
      word = as.character(w$word %||% ""),
      confidence = as.numeric(w$confidence %||% NA),
      stringsAsFactors = FALSE
    )
  }))
  
  return(words_df)
}


#' Null-coalescing Operator
#'
#' Returns the left-hand side if it is not NULL, otherwise returns the
#' right-hand side. Used internally for handling optional fields in
#' Deepgram responses.
#'
#' @param lhs Left-hand side value.
#' @param rhs Right-hand side value (default if lhs is NULL).
#'
#' @return \code{lhs} if not NULL, otherwise \code{rhs}.
#'
#' @keywords internal
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}


################################################################################
# AWS Transcribe Functions
################################################################################

#' Combine Two AWS Transcribe Outputs into Baseline Transcript
#'
#' Takes two AWS Transcribe outputs (parsed JSON responses from separate audio
#' files, one per speaker) and combines them into a single data frame ready
#' for processing with \code{\link{natural_turn_batch}} or
#' \code{\link{natural_turn_transcript}}.
#'
#' @param transcript_speaker1 List. The parsed JSON response from AWS Transcribe
#'   for speaker 1 (as returned by \code{jsonlite::fromJSON()} or
#'   \code{httr::content(response, "parsed")}).
#' @param transcript_speaker2 List. The parsed JSON response from AWS Transcribe
#'   for speaker 2.
#' @param speaker1_id Character. Identifier for speaker 1 (e.g., email address,
#'   name, or role). Default: "speaker1".
#' @param speaker2_id Character. Identifier for speaker 2. Default: "speaker2".
#' @param conversation_id Optional character. Identifier for the conversation.
#'   If provided, adds a \code{conversation_id} column to the output.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{speaker}{Character. The speaker identifier.}
#'   \item{start}{Numeric. Start time of the word in seconds.}
#'   \item{stop}{Numeric. End time of the word in seconds.}
#'   \item{text}{Character. The word with punctuation.}
#'   \item{word}{Character. The raw word content.}
#'   \item{confidence}{Numeric. AWS confidence score for the word.}
#'   \item{conversation_id}{Character. Conversation identifier (if provided).}
#' }
#'
#' @details
#' AWS Transcribe returns JSON with words in \code{results$items}. Each item
#' has \code{type} ("pronunciation" or "punctuation"), \code{start_time},
#' \code{end_time}, and \code{alternatives} with \code{content} and
#' \code{confidence}.
#'
#' This function only extracts "pronunciation" type items (actual words),
#' not punctuation items. Punctuation is attached to words when available.
#'
#' @examples
#' \dontrun{
#' # Read AWS Transcribe JSON outputs
#' speaker1_json <- jsonlite::fromJSON("speaker1_transcribe.json")
#' speaker2_json <- jsonlite::fromJSON("speaker2_transcribe.json")
#'
#' # Combine into baseline transcript (uses default speaker1/speaker2 IDs)
#' baseline <- combine_aws_transcripts(
#'   transcript_speaker1 = speaker1_json,
#'   transcript_speaker2 = speaker2_json
#' )
#'
#' # Process with NaturalTurn
#' result <- natural_turn_transcript(baseline)
#' }
#'
#' @seealso \code{\link{combine_transcripts}}
#'
#' @importFrom dplyr bind_rows arrange select any_of everything
#' @importFrom rlang .data
#' @keywords internal
combine_aws_transcripts <- function(transcript_speaker1,
                                    transcript_speaker2,
                                    speaker1_id = "speaker1",
                                    speaker2_id = "speaker2",
                                    conversation_id = NULL) {
  
  # Input validation
  if (!is.list(transcript_speaker1)) {
    stop("transcript_speaker1 must be a list (parsed AWS Transcribe JSON)")
  }
  if (!is.list(transcript_speaker2)) {
    stop("transcript_speaker2 must be a list (parsed AWS Transcribe JSON)")
  }
  
  # Extract words from each transcript
  words1 <- extract_aws_words(transcript_speaker1)
  words2 <- extract_aws_words(transcript_speaker2)
  
  # Check if both have words
  if (nrow(words1) == 0 && nrow(words2) == 0) {
    warning("Both transcripts are empty. Returning empty data frame.")
    return(create_empty_transcript_df())
  }
  
  # Add speaker identifiers
  if (nrow(words1) > 0) {
    words1$speaker <- speaker1_id
  }
  if (nrow(words2) > 0) {
    words2$speaker <- speaker2_id
  }
  
  # Combine and sort by start time
  combined <- dplyr::bind_rows(words1, words2)
  combined <- dplyr::arrange(combined, .data$start)
  
  # Reorder columns
  combined <- dplyr::select(combined, 
                            "speaker", "start", "stop", "text", 
                            dplyr::any_of(c("word", "confidence")))
  
  # Add conversation_id if provided
  if (!is.null(conversation_id)) {
    combined$conversation_id <- conversation_id
    combined <- dplyr::select(combined, 
                              "conversation_id", dplyr::everything())
  }
  
  return(combined)
}


#' Extract Words from AWS Transcribe Response
#'
#' Extracts the words data frame from an AWS Transcribe JSON response.
#' This is a helper function used by \code{\link{combine_aws_transcripts}}.
#'
#' @param aws_response List. The parsed JSON response from AWS Transcribe.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{start}{Numeric. Start time of the word in seconds.}
#'   \item{stop}{Numeric. End time of the word in seconds.}
#'   \item{text}{Character. The word content (with trailing punctuation if available).}
#'   \item{word}{Character. The raw word content.}
#'   \item{confidence}{Numeric. AWS confidence score for the word.}
#' }
#'
#' @details
#' AWS Transcribe structure: \code{results$items} contains a list of items
#' where each item has \code{type}, \code{start_time}, \code{end_time},
#' and \code{alternatives[[1]]$content} and \code{alternatives[[1]]$confidence}.
#'
#' Only items with \code{type == "pronunciation"} are extracted. Punctuation
#' items are attached to the preceding word.
#'
#' @examples
#' \dontrun{
#' aws_json <- jsonlite::fromJSON("transcribe_output.json")
#' words_df <- extract_aws_words(aws_json)
#' head(words_df)
#' }
#'
#' @seealso \code{\link{combine_transcripts}}
#'
#' @keywords internal
extract_aws_words <- function(aws_response) {
  
  # Navigate to items list
  items <- tryCatch({
    aws_response$results$items
  }, error = function(e) {
    NULL
  })
  
  # Return empty data frame if no items found
  if (is.null(items) || length(items) == 0) {
    return(data.frame(
      start = numeric(0),
      stop = numeric(0),
      text = character(0),
      word = character(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Handle both list and data frame formats from jsonlite
  if (is.data.frame(items)) {
    # Data frame format (from jsonlite with simplifyVector = TRUE)
    words_df <- extract_aws_words_df(items)
  } else {
    # List format
    words_df <- extract_aws_words_list(items)
  }
  
  return(words_df)
}


#' Extract AWS Words from Data Frame Format
#' @keywords internal
extract_aws_words_df <- function(items) {
  # Filter to pronunciation items only
  pron_idx <- which(items$type == "pronunciation")
  
  if (length(pron_idx) == 0) {
    return(data.frame(
      start = numeric(0),
      stop = numeric(0),
      text = character(0),
      word = character(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract words
  results <- lapply(pron_idx, function(i) {
    item <- items[i, ]
    
    # Get content and confidence from alternatives
    content <- ""
    confidence <- NA_real_
    
    if (!is.null(item$alternatives)) {
      alts <- item$alternatives
      if (is.data.frame(alts) && nrow(alts) > 0) {
        content <- as.character(alts$content[1] %||% "")
        confidence <- as.numeric(alts$confidence[1] %||% NA)
      } else if (is.list(alts) && length(alts) > 0) {
        content <- as.character(alts[[1]]$content %||% "")
        confidence <- as.numeric(alts[[1]]$confidence %||% NA)
      }
    }
    
    # Check if next item is punctuation and append it
    text <- content
    next_idx <- i + 1
    if (next_idx <= nrow(items) && items$type[next_idx] == "punctuation") {
      punct_alts <- items$alternatives[next_idx]
      if (is.data.frame(punct_alts) && nrow(punct_alts) > 0) {
        text <- paste0(content, punct_alts$content[1])
      } else if (is.list(punct_alts) && length(punct_alts) > 0) {
        text <- paste0(content, punct_alts[[1]]$content)
      }
    }
    
    data.frame(
      start = as.numeric(item$start_time %||% NA),
      stop = as.numeric(item$end_time %||% NA),
      text = text,
      word = content,
      confidence = confidence,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}


#' Extract AWS Words from List Format
#' @keywords internal
extract_aws_words_list <- function(items) {
  results <- list()
  
  for (i in seq_along(items)) {
    item <- items[[i]]
    
    # Skip non-pronunciation items
    if (is.null(item$type) || item$type != "pronunciation") {
      next
    }
    
    # Get content and confidence
    content <- ""
    confidence <- NA_real_
    
    if (!is.null(item$alternatives) && length(item$alternatives) > 0) {
      alt <- item$alternatives[[1]]
      content <- as.character(alt$content %||% "")
      confidence <- as.numeric(alt$confidence %||% NA)
    }
    
    # Check if next item is punctuation
    text <- content
    if (i < length(items)) {
      next_item <- items[[i + 1]]
      if (!is.null(next_item$type) && next_item$type == "punctuation") {
        if (!is.null(next_item$alternatives) && length(next_item$alternatives) > 0) {
          punct <- next_item$alternatives[[1]]$content %||% ""
          text <- paste0(content, punct)
        }
      }
    }
    
    results[[length(results) + 1]] <- data.frame(
      start = as.numeric(item$start_time %||% NA),
      stop = as.numeric(item$end_time %||% NA),
      text = text,
      word = content,
      confidence = confidence,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(results) == 0) {
    return(data.frame(
      start = numeric(0),
      stop = numeric(0),
      text = character(0),
      word = character(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, results)
}


################################################################################
# AssemblyAI Functions
################################################################################

#' Combine Two AssemblyAI Transcriptions into Baseline Transcript
#'
#' Takes two AssemblyAI API transcription outputs (parsed JSON responses from
#' separate audio files, one per speaker) and combines them into a single
#' data frame ready for processing with \code{\link{natural_turn_batch}} or
#' \code{\link{natural_turn_transcript}}.
#'
#' @param transcript_speaker1 List. The parsed JSON response from AssemblyAI
#'   for speaker 1 (as returned by \code{httr::content(response, "parsed")}).
#' @param transcript_speaker2 List. The parsed JSON response from AssemblyAI
#'   for speaker 2.
#' @param speaker1_id Character. Identifier for speaker 1. Default: "speaker1".
#' @param speaker2_id Character. Identifier for speaker 2. Default: "speaker2".
#' @param conversation_id Optional character. Identifier for the conversation.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{speaker}{Character. The speaker identifier.}
#'   \item{start}{Numeric. Start time of the word in seconds.}
#'   \item{stop}{Numeric. End time of the word in seconds.}
#'   \item{text}{Character. The word text.}
#'   \item{confidence}{Numeric. AssemblyAI confidence score for the word.}
#'   \item{conversation_id}{Character. Conversation identifier (if provided).}
#' }
#'
#' @details
#' AssemblyAI returns JSON with words in the \code{words} array. Each word
#' object has \code{text}, \code{start} (in milliseconds), \code{end}
#' (in milliseconds), and \code{confidence}.
#'
#' Note: AssemblyAI times are in milliseconds and are converted to seconds
#' by this function.
#'
#' @examples
#' \dontrun{
#' # Get AssemblyAI transcriptions (see AssemblyAI API docs)
#' speaker1_json <- httr::content(response1, "parsed")
#' speaker2_json <- httr::content(response2, "parsed")
#'
#' # Combine into baseline transcript (uses default speaker1/speaker2 IDs)
#' baseline <- combine_assemblyai_transcripts(
#'   transcript_speaker1 = speaker1_json,
#'   transcript_speaker2 = speaker2_json
#' )
#'
#' # Process with NaturalTurn
#' result <- natural_turn_transcript(baseline)
#' }
#'
#' @seealso \code{\link{combine_transcripts}}
#'
#' @importFrom dplyr bind_rows arrange select any_of everything
#' @importFrom rlang .data
#' @keywords internal
combine_assemblyai_transcripts <- function(transcript_speaker1,
                                           transcript_speaker2,
                                           speaker1_id = "speaker1",
                                           speaker2_id = "speaker2",
                                           conversation_id = NULL) {
  
  # Input validation
  if (!is.list(transcript_speaker1)) {
    stop("transcript_speaker1 must be a list (parsed AssemblyAI JSON)")
  }
  if (!is.list(transcript_speaker2)) {
    stop("transcript_speaker2 must be a list (parsed AssemblyAI JSON)")
  }
  
  # Extract words from each transcript
  words1 <- extract_assemblyai_words(transcript_speaker1)
  words2 <- extract_assemblyai_words(transcript_speaker2)
  
  # Check if both have words
  if (nrow(words1) == 0 && nrow(words2) == 0) {
    warning("Both transcripts are empty. Returning empty data frame.")
    return(create_empty_transcript_df())
  }
  
  # Add speaker identifiers
  if (nrow(words1) > 0) {
    words1$speaker <- speaker1_id
  }
  if (nrow(words2) > 0) {
    words2$speaker <- speaker2_id
  }
  
  # Combine and sort by start time
  combined <- dplyr::bind_rows(words1, words2)
  combined <- dplyr::arrange(combined, .data$start)
  
  # Reorder columns
  combined <- dplyr::select(combined, 
                            "speaker", "start", "stop", "text", 
                            dplyr::any_of(c("confidence")))
  
  # Add conversation_id if provided
  if (!is.null(conversation_id)) {
    combined$conversation_id <- conversation_id
    combined <- dplyr::select(combined, 
                              "conversation_id", dplyr::everything())
  }
  
  return(combined)
}


#' Extract Words from AssemblyAI Transcription Response
#'
#' Extracts the words data frame from an AssemblyAI API transcription response.
#' This is a helper function used by \code{\link{combine_assemblyai_transcripts}}.
#'
#' @param assemblyai_response List. The parsed JSON response from AssemblyAI.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{start}{Numeric. Start time of the word in seconds.}
#'   \item{stop}{Numeric. End time of the word in seconds.}
#'   \item{text}{Character. The word text.}
#'   \item{confidence}{Numeric. AssemblyAI confidence score for the word.}
#' }
#'
#' @details
#' AssemblyAI structure: \code{words} contains a list/array where each word
#' has \code{text}, \code{start}, \code{end}, and \code{confidence}.
#'
#' Important: AssemblyAI returns times in milliseconds. This function converts
#' them to seconds to match the expected format for NaturalTurn.
#'
#' @examples
#' \dontrun{
#' assemblyai_json <- httr::content(response, "parsed")
#' words_df <- extract_assemblyai_words(assemblyai_json)
#' head(words_df)
#' }
#'
#' @seealso \code{\link{combine_transcripts}}
#'
#' @keywords internal
extract_assemblyai_words <- function(assemblyai_response) {
  
  # Navigate to words list
  words_list <- tryCatch({
    assemblyai_response$words
  }, error = function(e) {
    NULL
  })
  
  # Return empty data frame if no words found
  if (is.null(words_list) || length(words_list) == 0) {
    return(data.frame(
      start = numeric(0),
      stop = numeric(0),
      text = character(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Handle both list and data frame formats
  if (is.data.frame(words_list)) {
    # Data frame format
    words_df <- data.frame(
      start = as.numeric(words_list$start) / 1000,  # ms to seconds
      stop = as.numeric(words_list$end) / 1000,     # ms to seconds
      text = as.character(words_list$text),
      confidence = as.numeric(words_list$confidence),
      stringsAsFactors = FALSE
    )
  } else {
    # List format
    words_df <- do.call(rbind, lapply(words_list, function(w) {
      data.frame(
        start = as.numeric(w$start %||% NA) / 1000,  # ms to seconds
        stop = as.numeric(w$end %||% NA) / 1000,     # ms to seconds
        text = as.character(w$text %||% ""),
        confidence = as.numeric(w$confidence %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  return(words_df)
}


################################################################################
# Generic/Unified Functions
################################################################################

#' Combine Two Transcriptions from Any Supported Provider
#'
#' A unified function that automatically detects the transcription provider
#' and combines two speaker transcriptions into a baseline transcript.
#'
#' @param transcript_speaker1 List. The parsed JSON response for speaker 1.
#' @param transcript_speaker2 List. The parsed JSON response for speaker 2.
#' @param provider Character. The transcription provider: "deepgram", "aws",
#'   or "assemblyai". If "auto" (default), the function attempts to detect
#'   the provider from the response structure.
#' @param speaker1_id Character. Identifier for speaker 1. Default: "speaker1".
#' @param speaker2_id Character. Identifier for speaker 2. Default: "speaker2".
#' @param conversation_id Optional character. Identifier for the conversation.
#'
#' @return A data frame with columns: speaker, start, stop, text, and
#'   provider-specific columns (word, confidence). Sorted by start time.
#'
#' @details
#' Provider detection heuristics:
#' \itemize{
#'   \item \strong{Deepgram}: Has \code{results$channels[[1]]$alternatives[[1]]$words}
#'   \item \strong{AWS Transcribe}: Has \code{results$items} with \code{type} field
#'   \item \strong{AssemblyAI}: Has \code{words} array with \code{start} in milliseconds
#' }
#'
#' @examples
#' \dontrun{
#' # Auto-detect provider and combine (uses default speaker1/speaker2 IDs)
#' baseline <- combine_transcripts(
#'   speaker1_response, speaker2_response
#' )
#'
#' # Or specify provider and custom speaker IDs
#' baseline <- combine_transcripts(
#'   speaker1_response, speaker2_response,
#'   provider = "aws",
#'   speaker1_id = "participant_A",
#'   speaker2_id = "participant_B"
#' )
#' }
#'
#' @seealso \code{\link{json_to_baseline}}
#'
#' @keywords internal
combine_transcripts <- function(transcript_speaker1,
                                transcript_speaker2,
                                provider = c("auto", "deepgram", "aws", "assemblyai"),
                                speaker1_id = "speaker1",
                                speaker2_id = "speaker2",
                                conversation_id = NULL) {
  
  provider <- match.arg(provider)
  
  # Auto-detect provider if needed
  if (provider == "auto") {
    provider <- detect_transcription_provider(transcript_speaker1)
    message(sprintf("Auto-detected provider: %s", provider))
  }
  
  # Call the appropriate function
  switch(provider,
    deepgram = combine_deepgram_transcripts(
      transcript_speaker1, transcript_speaker2,
      speaker1_id, speaker2_id, conversation_id
    ),
    aws = combine_aws_transcripts(
      transcript_speaker1, transcript_speaker2,
      speaker1_id, speaker2_id, conversation_id
    ),
    assemblyai = combine_assemblyai_transcripts(
      transcript_speaker1, transcript_speaker2,
      speaker1_id, speaker2_id, conversation_id
    ),
    stop("Unknown provider: ", provider)
  )
}


#' Detect Transcription Provider from Response Structure
#'
#' Examines the structure of a parsed transcription response and attempts
#' to identify which speech-to-text provider generated it.
#'
#' @param response List. A parsed JSON transcription response.
#'
#' @return Character string: "deepgram", "aws", "assemblyai", or "unknown".
#'
#' @keywords internal
detect_transcription_provider <- function(response) {
  
  # Check for Deepgram structure
  if (!is.null(response$results$channels)) {
    return("deepgram")
  }
  
  # Check for AWS Transcribe structure
  if (!is.null(response$results$items)) {
    return("aws")
  }
  
  # Check for AssemblyAI structure (has 'words' at top level with 'start' in ms)
  if (!is.null(response$words)) {
    # AssemblyAI uses milliseconds, check if start values are > 1000 (likely ms)
    if (is.list(response$words) && length(response$words) > 0) {
      first_word <- response$words[[1]]
      if (!is.null(first_word$start) && first_word$start > 1000) {
        return("assemblyai")
      }
    }
    if (is.data.frame(response$words) && nrow(response$words) > 0) {
      if (response$words$start[1] > 1000) {
        return("assemblyai")
      }
    }
  }
  
  warning("Could not auto-detect transcription provider. Please specify 'provider' explicitly.")
  return("unknown")
}


#' Create Empty Transcript Data Frame
#'
#' Returns an empty data frame with the standard transcript columns.
#' Used internally when transcripts are empty.
#'
#' @return An empty data frame with transcript columns.
#'
#' @keywords internal
create_empty_transcript_df <- function() {
  data.frame(
    speaker = character(0),
    start = numeric(0),
    stop = numeric(0),
    text = character(0),
    word = character(0),
    confidence = numeric(0),
    stringsAsFactors = FALSE
  )
}


################################################################################
# Utterance Collapsing Functions
################################################################################

#' Collapse Words into Utterances by Speaker
#'
#' Takes word-level transcript data and collapses consecutive words from the
#' same speaker into utterances. This produces a baseline transcript with
#' alternating speaker turns, similar to the output of process_transcript.R.
#'
#' @param word_df Data frame with word-level data. Must have columns:
#'   \code{speaker}, \code{start}, \code{stop}, \code{text}.
#' @param speaker_col Name of speaker column. Default: "speaker".
#' @param start_col Name of start time column. Default: "start".
#' @param stop_col Name of stop time column. Default: "stop".
#' @param text_col Name of text column. Default: "text".
#'
#' @return A data frame with one row per utterance (speaker turn):
#' \describe{
#'   \item{speaker}{Character. The speaker identifier.}
#'   \item{start}{Numeric. Start time of the utterance in seconds.}
#'   \item{stop}{Numeric. End time of the utterance in seconds.}
#'   \item{text}{Character. The concatenated text of all words in the utterance.}
#'   \item{n_words}{Integer. Number of words in the utterance.}
#'   \item{duration}{Numeric. Duration of the utterance in seconds.}
#'   \item{speech_turn}{Integer. Sequential turn number.}
#' }
#'
#' @details
#' This function implements the same logic as \code{process_transcript.R}:
#' consecutive words from the same speaker are collapsed into a single
#' utterance. When the speaker changes, a new utterance begins.
#'
#' If \code{conversation_id} column exists in the input, it is preserved.
#'
#' @examples
#' \dontrun{
#' # Get word-level data from Deepgram
#' words <- combine_deepgram_transcripts(speaker1, speaker2)
#'
#' # Collapse into utterances
#' utterances <- collapse_words_to_utterances(words)
#'
#' # Process with NaturalTurn
#' result <- natural_turn_transcript(utterances)
#' }
#'
#' @seealso \code{\link{json_to_baseline}}
#'
#' @importFrom dplyr arrange mutate group_by summarise n first last
#' @importFrom rlang .data
#' @keywords internal
collapse_words_to_utterances <- function(word_df,
                                         speaker_col = "speaker",
                                         start_col = "start",
                                         stop_col = "stop",
                                         text_col = "text") {
  
  if (!is.data.frame(word_df) || nrow(word_df) == 0) {
    warning("Empty input, returning empty data frame")
    return(data.frame(
      speaker = character(0),
      start = numeric(0),
      stop = numeric(0),
      text = character(0),
      n_words = integer(0),
      duration = numeric(0),
      speech_turn = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Standardize column names
  df <- word_df
  names(df)[names(df) == speaker_col] <- "speaker"
  names(df)[names(df) == start_col] <- "start"
  names(df)[names(df) == stop_col] <- "stop"
  names(df)[names(df) == text_col] <- "text"
  
  # Sort by start time
  df <- df[order(df$start), ]
  
  # Identify speaker changes to create utterance groups
  df$speaker_changed <- c(TRUE, df$speaker[-1] != df$speaker[-nrow(df)])
  df$utterance_id <- cumsum(df$speaker_changed)
  
  # Check if conversation_id exists
  has_conv_id <- "conversation_id" %in% names(df)
  
  # Collapse words into utterances
  result <- df %>%
    dplyr::group_by(.data$utterance_id) %>%
    dplyr::summarise(
      speaker = dplyr::first(.data$speaker),
      start = min(.data$start),
      stop = max(.data$stop),
      text = paste(.data$text, collapse = " "),
      n_words = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$start) %>%
    dplyr::mutate(
      duration = .data$stop - .data$start,
      speech_turn = dplyr::row_number()
    ) %>%
    dplyr::select(-.data$utterance_id)
  
  # Add conversation_id if it existed in input
  if (has_conv_id) {
    conv_id <- df$conversation_id[1]
    result$conversation_id <- conv_id
    result <- dplyr::select(result, .data$conversation_id, dplyr::everything())
  }
  
  return(as.data.frame(result))
}


#' Convert JSON Transcriptions to Baseline Transcript
#'
#' Converts two JSON transcription outputs (one per speaker) into a baseline
#' transcript with one row per utterance. Supports Deepgram, AWS Transcribe,
#' and AssemblyAI (auto-detected).
#'
#' @param transcript_speaker1 List. Parsed JSON for speaker 1.
#' @param transcript_speaker2 List. Parsed JSON for speaker 2.
#' @param speaker1_id Character. Identifier for speaker 1. Default: "speaker1".
#' @param speaker2_id Character. Identifier for speaker 2. Default: "speaker2".
#' @param conversation_id Optional character. Conversation identifier.
#' @param provider Character. Provider: "auto", "deepgram", "aws", or "assemblyai".
#'   Default: "auto" (auto-detects from JSON structure).
#'
#' @return A data frame with one row per utterance (speaker turn):
#' \describe{
#'   \item{speaker}{Speaker identifier}
#'   \item{start}{Start time in seconds}
#'   \item{stop}{End time in seconds}
#'   \item{text}{Utterance text}
#'   \item{n_words}{Word count}
#'   \item{duration}{Duration in seconds}
#'   \item{speech_turn}{Sequential turn number}
#' }
#'
#' @examples
#' \dontrun{
#' # Convert JSON to baseline transcript
#' baseline <- json_to_baseline(speaker1_json, speaker2_json)
#'
#' # Process with NaturalTurn
#' result <- baseline_to_naturalturn(baseline, output_csv = "result.csv")
#' }
#'
#' @seealso \code{\link{baseline_to_naturalturn}}
#'
#' @export
json_to_baseline <- function(transcript_speaker1,
                             transcript_speaker2,
                             speaker1_id = "speaker1",
                             speaker2_id = "speaker2",
                             conversation_id = NULL,
                             provider = c("auto", "deepgram", "aws", "assemblyai")) {
  
  provider <- match.arg(provider)
  
  # First combine word-level data
  words <- combine_transcripts(
    transcript_speaker1,
    transcript_speaker2,
    provider = provider,
    speaker1_id = speaker1_id,
    speaker2_id = speaker2_id,
    conversation_id = conversation_id
  )
  
  # Then collapse into utterances
  utterances <- collapse_words_to_utterances(words)
  
  return(utterances)
}


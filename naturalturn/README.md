# naturalturn

An R package implementing the NaturalTurn algorithm for analyzing conversational transcripts.

## Description

This package is an **R implementation** of the NaturalTurn algorithm from the Python companion repository: https://github.com/betterup/natural-turn-transcription

The NaturalTurn algorithm identifies primary turns and overlapping speech (backchannels/secondary turns) in conversational transcripts. It collapses short pauses within the same speaker while preserving overlaps between different speakers, and outputs data in wide format with one row per primary turn and listener overlaps in columns.

The package implements:
- **NaturalTurn algorithm** (Cooney & Reece, 2025) for turn segmentation and backchannel detection
- **Interruption detection and within-speech pause tracking** (Di Stasi et al., 2024)

## Installation

```r
# Install from source
devtools::install("naturalturn")
```

## Quick Start

```r
library(naturalturn)

# Process a single conversation
transcript <- data.frame(
  speaker = c("A", "A", "B", "A", "B"),
  text = c("Hello", "there", "Hi", "How are you", "Good"),
  time.s = c(0.0, 1.2, 1.5, 3.0, 3.5),
  time.e = c(1.0, 2.0, 2.5, 4.0, 4.2)
)

result <- collapse_turns_natural_wide(transcript, max_pause = 1.5)

# Process multiple conversations from a CSV file
result <- process_batch_wide(
  input_csv = "transcripts.csv",
  output_csv = "transcripts_processed.csv",
  max_pause = 1.5,
  backchannel_word_max = 3,
  backchannel_proportion = 0.5
)
```

## Main Functions

- `collapse_turns_natural_wide()`: Main function to collapse turns into wide format
- `process_batch_wide()`: Batch process multiple conversations from a CSV file
- `collapse_turns_preserving_overlaps()`: Core function to collapse short pauses while preserving overlaps

## Parameters

- `max_pause`: Maximum pause (seconds) to merge same-speaker segments (default: 1.5)
- `backchannel_word_max`: Max words for backchannel classification (default: 3)
- `backchannel_proportion`: Min % words that must be backchannel cues (default: 0.5)
- `interruption_duration_min`: Min duration for interruption flag (default: 6.0)
- `interruption_lag_duration_min`: Min previous turn duration for interruption (default: 1.0)

## Output Format

The wide format output includes:
- **Primary speaker columns**: `speaker`, `start`, `stop`, `duration`, `utterance`, `responseTime`, `interruption`, etc.
- **Listener overlap columns**: `n_listener_turns`, `overlap_speaker`, `utterance_listener` (concatenated), and `*_listener_list` columns (lists of individual listener turns)

## References

- Cooney, G., & Reece, A. (2025). NaturalTurn: A method to segment speech into psychologically meaningful conversational turns. *Scientific Reports*, 15, 39155. https://doi.org/10.1038/s41598-025-24381-1

- Di Stasi, M., Templeton, E., & Quoidbach, J. (2024). Zooming out on bargaining tables: Exploring which conversation dynamics predict negotiation outcomes. *Journal of Applied Psychology*, 109, 1077-1093.

## Python Companion Repository

This R package is an implementation of the Python NaturalTurn repository:
https://github.com/betterup/natural-turn-transcription

## License

MIT

## Author

Jordi Quoidbach

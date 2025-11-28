# naturalturn

An R package implementing the NaturalTurn algorithm for analyzing conversational transcripts.

## Description

This package is an **R implementation** of the NaturalTurn algorithm (Cooney & Reece, 2025) for segmenting conversational transcripts into psychologically meaningful turns. It is based on the Python companion repository: https://github.com/betterup/natural-turn-transcription

The NaturalTurn algorithm identifies primary turns and overlapping speech (backchannels/secondary turns), collapsing short pauses within the same speaker while preserving overlaps between different speakers.

**Additionally**, the package integrates **interruption detection** and **within-speech pause tracking** algorithms from Di Stasi, Templeton, & Quoidbach (2024).

### Key Features
- **NaturalTurn algorithm** (Cooney & Reece, 2025): Turn segmentation and backchannel detection
- **Interruption detection** (Di Stasi, Templeton, & Quoidbach, 2024): Flags turns that interrupt the previous speaker
- **Within-speech pause tracking** (Di Stasi, Templeton, & Quoidbach, 2024): Tracks long pauses within a speaker's turn

## Installation

```r
# Install from GitHub
devtools::install_github("JordiQuoidbach/naturalturn", subdir = "naturalturn")
```

## Usage

### Processing a Single Transcript

Use `natural_turn_transcript()` to process a single conversation:

```r
library(naturalturn)

# Your transcript must have: speaker, text, start time, and end time columns
transcript <- data.frame(
  speaker = c("A", "A", "B", "A", "B"),
  text = c("Hello", "there", "Hi", "How are you", "Good"),
  time.s = c(0.0, 1.2, 1.5, 3.0, 3.5),
  time.e = c(1.0, 2.0, 2.5, 4.0, 4.2)
)

# Process with default parameters
result <- natural_turn_transcript(transcript)

# Or customize parameters
result <- natural_turn_transcript(
  transcript,
  max_pause = 1.5,
  backchannel_word_max = 3,
  backchannel_proportion = 0.5,
  interruption_duration_min = 6.0,
  interruption_lag_duration_min = 1.0
)
```

### Processing Multiple Conversations (Batch)

Use `natural_turn_batch()` to process a data frame with multiple conversations:

```r
# Load your data
transcripts <- read.csv("transcripts.csv")

# Process all conversations
result <- natural_turn_batch(
  transcripts,
  conversation_id_col = "conversation_id",
  speaker_col = "speaker"
)

# Optionally save to CSV
result <- natural_turn_batch(
  transcripts,
  output_csv = "transcripts_processed.csv",
  conversation_id_col = "conversation_id"
)
```

Your data frame must contain a column to identify different conversations (e.g., `conversation_id` or `nego_id`).

## Parameters

### Turn Segmentation (NaturalTurn Algorithm)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `max_pause` | 1.5 | Maximum pause (in seconds) between consecutive speech segments from the **same speaker** to be merged into one turn. If Speaker A speaks, pauses < `max_pause`, then speaks again (without Speaker B interrupting), it's treated as ONE turn. |
| `backchannel_word_max` | 3 | Maximum number of words for an overlapping utterance to be classified as a "backchannel" (vs "secondary"). Example: "Yeah okay" (2 words) can be a backchannel; "I completely agree with you" (5 words) is secondary. |
| `backchannel_proportion` | 0.5 | Minimum proportion (0-1) of words that must be backchannel cues (e.g., "yeah", "okay", "mhm") for an utterance to be classified as a backchannel. Example: "Yeah really" (2/2 = 100%) → backchannel; "Yeah I think" (1/3 = 33%) → secondary. |

### Interruption Detection (Di Stasi et al., 2024)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `interruption_duration_min` | 6.0 | Minimum duration (in seconds) for a turn to be considered an interruption. Only turns longer than this can be flagged as interruptions. |
| `interruption_lag_duration_min` | 1.0 | Minimum duration (in seconds) of the **previous** turn for the current turn to be an interruption. Ensures the interrupted turn was substantial (not just a brief utterance). |

**Interruption criteria** (all must be true):
1. Current turn duration > `interruption_duration_min`
2. Current turn starts before previous turn ends (overlap/negative pause)
3. Previous turn duration > `interruption_lag_duration_min`
4. Current speaker is different from previous speaker

### Column Name Parameters

For `natural_turn_transcript()`:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `speaker_col` | "speaker" | Name of the column containing speaker identifiers |
| `text_col` | "text" | Name of the column containing utterance text |
| `start_col` | "time.s" | Name of the column containing start times (in seconds) |
| `stop_col` | "time.e" | Name of the column containing end times (in seconds) |

For `natural_turn_batch()` (additional):

| Parameter | Default | Description |
|-----------|---------|-------------|
| `conversation_id_col` | auto-detect | Column identifying different conversations. Auto-detects "conversation_id" or "nego_id". |

## Output Format

The output is a data frame in **wide format** with one row per primary turn:

### Primary Speaker Columns

| Column | Description |
|--------|-------------|
| `speaker` | Speaker identifier |
| `start` | Turn start time (seconds) |
| `stop` | Turn end time (seconds) |
| `duration` | Turn duration (seconds) |
| `utterance` | The spoken text |
| `responseTime` | Time gap before this turn (can be negative for overlaps) |
| `interruption` | 1 if this turn interrupts the previous speaker, 0 otherwise |
| `n_words_speaker` | Word count |
| `n_questions_speaker` | Number of questions (?) |
| `ends_with_question_speaker` | TRUE if turn ends with a question |
| `n_utterances_merged_speaker` | Number of original segments merged into this turn |

### Within-Turn Pause Columns (Di Stasi et al., 2024)

| Column | Description |
|--------|-------------|
| `n_segments` | Number of speech segments merged into this turn |
| `n_long_pauses` | Number of pauses ≥ `max_pause` within the turn |
| `internal_pauses` | List of all internal pause durations |
| `internal_pause_1`, `internal_pause_2`, ... | Individual long pause durations |

### Listener/Overlap Columns

| Column | Description |
|--------|-------------|
| `n_listener_turns` | Number of listener turns during this primary turn |
| `overlap_speaker` | TRUE if there was overlapping speech |
| `utterance_listener` | Concatenated listener utterances |
| `utterance_listener_list` | List of individual listener utterances |
| `utterance_type_listener_list` | List of types ("backchannel" or "secondary") |
| `start_listener_list`, `stop_listener_list` | Timing of listener turns |
| `duration_listener_list` | Durations of listener turns |

## Backchannel Detection

The algorithm uses word lists to classify overlapping speech:

**Backchannel cues** (indicate listener engagement without taking the floor):
> "yeah", "okay", "mhm", "right", "uh huh", "wow", "really", "sure", "exactly", "gotcha", ...

**Not backchannel cues** (indicate the listener is taking the floor):
> "and", "but", "i", "well", "so", "like", "you", "we", ...

Classification rules:
1. If **every** word is a backchannel cue → BACKCHANNEL (regardless of length)
2. If > `backchannel_word_max` words → SECONDARY
3. If starts with a "not backchannel" word → SECONDARY
4. If ≥ `backchannel_proportion` of words are backchannel cues → BACKCHANNEL
5. Otherwise → SECONDARY

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

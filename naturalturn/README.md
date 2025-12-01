# naturalturn

An R package for processing conversational transcripts into analysis-ready turn-by-turn data using the NaturalTurn algorithm.

## Description

This package transforms raw speech-to-text transcriptions into psychologically meaningful conversational turns ready for analysis. It supports two workflows:

### Workflow 1: From Raw API JSON (Separate Speaker Audio)
If you have **separate audio files per speaker** (common with Zoom, Teams, podcasts):
1. **`json_to_baseline()`** — Combines two speaker JSON transcriptions (from Deepgram, AWS Transcribe, or AssemblyAI) into a unified turn-by-turn baseline transcript
2. **`baseline_to_naturalturn()`** — Processes the baseline into NaturalTurn format with backchannel detection, interruptions, and pause tracking

### Workflow 2: From Existing Transcript
If you already have a **timestamped turn-by-turn transcript** (e.g., from a transcription service with speaker diarization):
1. **`baseline_to_naturalturn()`** — Directly process your transcript into NaturalTurn format

### Current Limitations
⚠️ **This version supports 1-on-1 conversations only** (dyads). Multi-party conversations are not yet supported.

---

The package implements the **NaturalTurn algorithm** (Cooney & Reece, 2025), which identifies primary turns and overlapping speech (backchannels/secondary turns), collapsing short pauses within the same speaker while preserving overlaps between different speakers.

**Additionally**, the package integrates **interruption detection** and **within-speech pause tracking** algorithms from Di Stasi, Templeton, & Quoidbach (2024).

Based on the Python companion repository: https://github.com/betterup/natural-turn-transcription

### Key Features
- **NaturalTurn algorithm** (Cooney & Reece, 2025): Turn segmentation and backchannel detection
- **Interruption detection** (Di Stasi, Templeton, & Quoidbach, 2024): Flags turns that interrupt the previous speaker
- **Within-speech pause tracking** (Di Stasi, Templeton, & Quoidbach, 2024): Tracks long pauses within a speaker's turn
- **Multi-provider support**: Auto-detects and processes JSON from Deepgram, AWS Transcribe, and AssemblyAI

## Installation

```r
# Install from GitHub
devtools::install_github("JordiQuoidbach/naturalturn", subdir = "naturalturn")
```

## Usage

### Processing Separate Speaker Audio Files

If you have separate audio files for each speaker (common with Zoom, Teams, etc.):

```r
library(naturalturn)
library(jsonlite)

# Load transcription JSON files (from Deepgram, AWS Transcribe, or AssemblyAI)
speaker1_json <- jsonlite::fromJSON("speaker1.json")
speaker2_json <- jsonlite::fromJSON("speaker2.json")

# Step 1: JSON → Baseline transcript
baseline <- json_to_baseline(speaker1_json, speaker2_json)

# Step 2: Baseline → NaturalTurn output
result <- baseline_to_naturalturn(baseline, output_csv = "result.csv")
```

**Supported providers** (auto-detected):
- Deepgram
- AWS Transcribe  
- AssemblyAI

**Custom speaker IDs** (optional):
```r
baseline <- json_to_baseline(
  speaker1_json, speaker2_json,
  speaker1_id = "participant_A",
  speaker2_id = "participant_B"
)
```

### Processing a Single Transcript

Use `baseline_to_naturalturn()` to process a single conversation:

```r
library(naturalturn)

# Your transcript must have: speaker, text, start time, and end time columns
baseline <- data.frame(
  speaker = c("A", "A", "B", "A", "B"),
  text = c("Hello", "there", "Hi", "How are you", "Good"),
  start = c(0.0, 1.2, 1.5, 3.0, 3.5),
  stop = c(1.0, 2.0, 2.5, 4.0, 4.2)
)

# Process - wide format (default)
result <- baseline_to_naturalturn(baseline)

# Save to CSV (list columns automatically converted to JSON strings)
result <- baseline_to_naturalturn(baseline, output_csv = "result.csv")

# Process in long format (one row per turn, including secondary/backchannels)
result_long <- baseline_to_naturalturn(baseline, output_format = "long")

```

### Processing Multiple Conversations (Batch)

Use `baseline_to_naturalturn_batch()` to process a data frame with multiple conversations:

```r
# Load your baseline data
baselines <- read.csv("baselines.csv")

# Process all conversations - wide format (default)
result <- baseline_to_naturalturn_batch(
  baselines,
  conversation_id_col = "conversation_id",
  speaker_id_col = "speaker"
)

# Process all conversations - long format
result_long <- baseline_to_naturalturn_batch(
  baselines,
  conversation_id_col = "conversation_id",
  speaker_id_col = "speaker",
  output_format = "long"
)

# Save to CSV
result <- baseline_to_naturalturn_batch(
  baselines,
  conversation_id_col = "conversation_id",
  speaker_id_col = "speaker",
  output_csv = "processed.csv"
)
```

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

### Within-Turn Pause Detection (Heldner & Edlund, 2010)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `short_pause_threshold` | 0.18 | Minimum pause duration (in seconds) to be considered a true pause. The 180ms threshold distinguishes pauses from stop closures (brief airflow blockages for consonants). |

**Pause classification:**
- **Short pauses**: >= 180ms but < `max_pause` (within collapsed segments)
- **Long pauses**: >= `max_pause` (between segments that weren't collapsed)

**Interruption criteria** (all must be true):
1. Current turn duration > `interruption_duration_min`
2. Current turn starts before previous turn ends (overlap/negative pause)
3. Previous turn duration > `interruption_lag_duration_min`
4. Current speaker is different from previous speaker

### Column Name Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `conversation_id_col` | *required* | Column identifying different conversations (batch only) |
| `speaker_id_col` | "speaker" | Column containing speaker identifiers |
| `text_col` | "text" | Column containing utterance text |
| `start_col` | "start" | Column containing start times (in seconds) |
| `stop_col` | "stop" | Column containing end times (in seconds) |
| `output_csv` | NULL | Optional path to save results as CSV (list columns auto-converted to JSON) |
| `output_format` | "wide" | Output format: "wide" (one row per primary turn) or "long" (one row per turn) |

### Speech-to-Text Integration Parameters

Parameters for `json_to_baseline()`:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `transcript_speaker1` | *required* | Parsed JSON response for speaker 1 |
| `transcript_speaker2` | *required* | Parsed JSON response for speaker 2 |
| `speaker1_id` | "speaker1" | Identifier for speaker 1 |
| `speaker2_id` | "speaker2" | Identifier for speaker 2 |
| `conversation_id` | NULL | Optional conversation identifier |
| `provider` | "auto" | Provider: "auto", "deepgram", "aws", or "assemblyai" |

**Supported providers** (auto-detected from JSON structure):
- **Deepgram** - Uses `punctuated_word` for text
- **AWS Transcribe** - Handles pronunciation + punctuation items
- **AssemblyAI** - Times in milliseconds (auto-converted to seconds)

## Output Formats

Choose between **wide** and **long** output formats depending on your analysis needs:

- **Wide format** (`output_format = "wide"`): One row per primary turn, with listener/overlap information in columns. Best for analyzing speaker turns, response times, and interruptions.

- **Long format** (`output_format = "long"`): One row per turn (both primary and secondary/backchannel). Best for utterance-level analysis including all backchannels and overlapping speech.

### Wide Format (default)

One row per primary turn, with listener overlaps in columns:

### Primary Speaker Columns

| Column | Description |
|--------|-------------|
| `speaker` | Speaker identifier |
| `start` | Turn start time (seconds) |
| `stop` | Turn end time (seconds) |
| `duration` | Turn duration (seconds) |
| `utterance` | The spoken text |
| `response_time` | Time gap before this turn (can be negative for overlaps) |
| `interruption` | 1 if this turn interrupts the previous speaker, 0 otherwise |
| `n_words_speaker` | Word count |
| `n_questions_speaker` | Number of questions (?) |
| `ends_with_question_speaker` | TRUE if turn ends with a question |
| `n_utterances_merged_speaker` | Number of original segments merged into this turn |

### Within-Turn Pause Columns (Di Stasi et al., 2024; Heldner & Edlund, 2010)

| Column | Description |
|--------|-------------|
| `n_segments` | Number of speech segments merged into this turn |
| `n_short_pauses` | Number of short pauses (>= 180ms, < `max_pause`) |
| `n_long_pauses` | Number of long pauses (>= `max_pause`) |
| `short_pauses` | List of short pause durations |
| `long_pauses` | List of long pause durations |
| `short_pause_1`, `short_pause_2`, ... | Individual short pause durations |
| `long_pause_1`, `long_pause_2`, ... | Individual long pause durations |

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

### Long Format

One row per turn (both primary and secondary/backchannel):

| Column | Description |
|--------|-------------|
| `turn_id` | Turn ID (secondary turns share ID with their overlapping primary turn) |
| `speaker` | Speaker identifier |
| `start` | Turn start time (seconds) |
| `stop` | Turn end time (seconds) |
| `duration` | Turn duration (seconds) |
| `utterance` | The spoken text |
| `utterance_type` | "primary", "secondary", or "backchannel" |
| `is_primary` | TRUE for primary turns, FALSE for secondary/backchannel |
| `response_time` | Time gap before this turn (can be negative for overlaps) |
| `n_words` | Word count |
| `n_questions` | Number of questions (?) |
| `ends_with_question` | TRUE if turn ends with a question |
| `n_utterances_merged` | Number of original segments merged |
| `n_segments` | Number of speech segments in this turn |
| `n_short_pauses` | Number of short pauses (>= 180ms, < `max_pause`) |
| `n_long_pauses` | Number of long pauses (>= `max_pause`) |
| `short_pauses` | List of short pause durations |
| `long_pauses` | List of long pause durations |

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

- Heldner, M., & Edlund, J. (2010). Pauses, gaps and overlaps in conversations. *Journal of Phonetics*, 38(4), 555-568.

## Python Companion Repository

This R package is an implementation of the Python NaturalTurn repository:
https://github.com/betterup/natural-turn-transcription

## License

MIT

## Authors

Jordi Quoidbach & Gus Cooney

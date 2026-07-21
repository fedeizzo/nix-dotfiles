# Calliope

Calliope is an AI-powered audiobook creation tool written in Rust. It automatically converts EPUB ebooks into high-quality `.m4b` audiobooks by using local Large Language Models (LLMs) to identify character dialogue and emotional contexts, and then routing that metadata to local Text-to-Speech (TTS) engines (like Qwen3-TTS via `audio.cpp`).

## Features

- **EPUB Parsing:** Automatically extracts chapters and cover art from standard EPUB files.
- **Smart Dialogue Tagging:** Uses a local LLM (OpenAI-compatible endpoints) to read paragraphs, identify who is speaking, and label the emotional tone (e.g., Happy, Angry, Neutral).
- **Emotion-Aware TTS:** Forwards the emotion as a prompt (`"Read with a happy tone"`) to TTS engines like Qwen3 to bring the audiobook to life.
- **In-Memory Crossfading:** Uses pure Rust (`hound`) to stitch and crossfade (30ms) audio boundaries to eliminate popping or clicking between sentences.
- **M4B Generation:** Packages all chapter files, timestamps, and the original EPUB cover image into a final iOS/Android-ready `.m4b` file using `ffmpeg`.
- **Fault-Tolerant Resuming:** Automatically resumes from where it left off if generation is interrupted.
- **Automatic API Retries:** Exponential backoff for network hiccups with local LLMs and TTS servers.

## Installation

Ensure you have Rust installed, and `ffmpeg` available on your system path.

```bash
cargo build --release
```

## Configuration

Calliope is primarily configured using a `config.toml` file (defaults to `./config.toml`).

```toml
[llm]
base_url = "https://your-llm-server.com/v1"
model = "qwen-nothink"
emotion_strategy = "metadata" # Use "metadata" or "inline"
language = "Italian"

[tts]
engine = "openai"
base_url = "https://your-tts-server.com/v1"
model = "qwen3-tts"
language = "Italian"

[tts.voices]
narrator = "narrator_ita"
"Rand" = "narrator_ita"
"Mat" = "narrator_ita"
```

You can assign specific characters from your book to specific TTS voice presets in the `[tts.voices]` block. `narrator` is the default fallback.

## Usage

```bash
cargo run -- convert --input /path/to/book.epub --output ./out
```

All CLI flags can override `config.toml` options:
- `--input` (Required): Path to the `.epub`.
- `--output` (Required): Directory to store `.wav` files and the final `.m4b`.
- `--language`: Sets the language for the LLM and TTS.
- `--dry-run`: Parses the EPUB and LLM but skips audio synthesis.

## How It Works

1. **Phase 1 (Chunking):** The EPUB text is cleaned and split into manageable 1500-character chunks.
2. **Phase 2 (Annotation):** Calliope sends the chunk to an LLM, asking for a structured JSON response identifying the speaker and their emotion for every sentence.
3. **Phase 3 (Synthesis):** Calliope maps the character to a voice clone, constructs the payload (injecting the emotion prompt), and queries the TTS engine. The audio chunks are faded by 30ms and concatenated in memory.
4. **Phase 4 (Packaging):** Calliope mathematically calculates the millisecond timestamps of each chapter, exports an `FFMETADATA1` manifest, extracts the EPUB cover art, and invokes `ffmpeg` to build the `.m4b` file.

# Calliope Supported TTS Models

This document outlines the supported Text-To-Speech (TTS) models within the Calliope ecosystem and specifies the recommended `--emotion-strategy` for each. 

Calliope supports two primary emotion extraction strategies:
1. **`inline`**: Injects `[emotion]` tags directly into the text sequence. Best for LLM-based TTS or autoregressive models that treat emotion tags as prompt tokens.
2. **`metadata`**: Returns a single global emotion tag per line. Best for traditional acoustic models, models requiring SSML wrappers, or models that require specific reference audio per emotion.

---

## Models Recommended for `inline` Strategy

These modern AR (autoregressive) models, LLM-based speech models, and expressive dialogue engines natively understand inline prompt text tokens for pacing and tonal shifts.

| Family | Supported variant(s) | Notes |
|---|---|---|
| **outetts** | Llama-OuteTTS-1.0-1B | Llama-based AR architecture easily interprets text-based prompt tokens. |
| **qwen3_tts** | Qwen3-TTS-12Hz (0.6B/1.7B) | AR TTS with voice design capabilities; highly responsive to inline text cues. |
| **vibevoice** | VibeVoice-1.5B, VibeVoice-7B | Multi-speaker dialogue TTS natively built to handle shifting tones mid-sentence. |
| **vevo2** | Vevo2 with Qwen2.5-0.5B AR | Text-to-Audio LLM backbone handles expressive inline singing/editing tags. |
| **omnivoice** | OmniVoice, Qwen3-0.6B based | Understands complex inline voice design instructions. |
| **index_tts2** | IndexTTS-2 | Specifically handles "expressive speech" variations well via inline text hinting. |
| **voxcpm2** | VoxCPM2-2B, 48 kHz | Large parameter voice design model. |

*Run command:*
`cargo run -- convert --input <file> --emotion-strategy inline --tts-engine <model>`

---

## Models Recommended for `metadata` Strategy

These models (often lightweight, VITS-based, or traditional voice cloning backends) generally prefer clean text inputs without bracketed tags. They rely on external APIs, SSML wrappers, or emotion-specific reference audio to control tone.

| Family | Supported variant(s) | Notes |
|---|---|---|
| **chatterbox** | Chatterbox with 0.5B backbone | Clean text input preferred; tone is dictated by voice cloning reference. |
| **pocket_tts** | PocketTTS-100M | Extremely lightweight; does not parse inline tags well. Use metadata mapping. |
| **moss_tts_nano** | MOSS-TTS-Nano-100M | Nano-scale model. Needs clean text. |
| **moss_tts_local** | MOSS-TTS-Local-Transformer | Standard Transformer TTS. Requires external emotion conditions. |
| **miotts** | MioTTS-1.7B | Voice cloning backend; use metadata to map to specific clone references. |
| **irodori_tts** | Irodori-TTS-500M-v3 | Japanese-focused voice design; map global emotion to voice presets. |
| **supertonic** | Supertonic 3 | Clean text inputs yield the most stable voice generation. |

*Run command:*
`cargo run -- convert --input <file> --emotion-strategy metadata --tts-engine <model>`

---

## Implementing the TTS Engine Trait

When implementing the `TtsEngine` trait in Rust for any of these models, consult the strategy list:
- If `metadata`, use the `AnnotatedLine.emotion` enum to select the reference audio (e.g., `assets/voices/rand_angry.wav`).
- If `inline`, pass the raw `AnnotatedLine.text` string (which contains `[angry]`, `[whispering]`, etc.) directly into the TTS inference API.

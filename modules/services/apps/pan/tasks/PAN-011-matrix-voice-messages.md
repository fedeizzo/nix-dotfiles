---
id: PAN-011
title: Accept Matrix voice messages through local ASR
status: backlog
priority: P2
owner: unassigned
created: 2026-10-24
updated: 2026-10-24
labels:
  - matrix
  - audio
  - asr
depends_on: []
related: []
---

# PAN-011: Accept Matrix voice messages through local ASR

## Summary

Let the user send voice messages to Pan in Matrix. Transcribe audio locally using llama-swap's `qwen3-asr` model, then process the transcript as a normal chat message.

## Context

Pan currently handles text messages in [the Matrix interface](../src/interface/matrix.rs). The homelab already configures `qwen3-asr` in [llama-swap](../../../ai-workflows/llama-swap/llama-swap.n.nix). The user wants to talk to Pan using recorded audio.

## Scope

- Receive Matrix audio attachments and voice messages, including encrypted media through the Matrix SDK.
- Check the allowed sender and room before downloading or transcribing audio.
- Add an application transcription port with a configurable local ASR adapter; verify the actual endpoint and accepted formats provided by the pinned server.
- Decode common Matrix voice formats with bounded duration, download size, conversion resources, concurrency, and request deadlines.
- Return the transcript in the originating room/thread and route it through existing chat and tool authorization flows.
- Keep raw audio temporary, clean up after success/failure, and avoid audio/transcript logging by default.
- Document configuration and useful failure messages.

## Out of Scope

- Live calls, wake words, always-on microphone access, or spoken replies.
- Sending audio to external transcription providers.
- Bypassing existing confirmations or granting extra tool authority to voice requests.

## Acceptance Criteria

- [ ] An authorized Matrix voice message is transcribed using local `qwen3-asr` and receives Pan's text response in the correct conversation.
- [ ] Encrypted voice attachments work; common client formats are tested.
- [ ] Unauthorized senders/rooms cause no media download, transcription, or tool execution.
- [ ] Duplicate events do not cause duplicate tool actions; ambiguous transcripts use existing clarification/confirmation flows.
- [ ] Empty, malformed, oversized, unsupported, or overlong audio and unavailable/slow ASR produce clear errors without crashing Pan.
- [ ] Temporary audio is removed on success/failure and stale files are cleaned after restart; logs omit audio and transcripts.
- [ ] Configuration and a manual Matrix smoke test are documented.

## Implementation Notes

Keep Matrix media handling in the interface adapter and ASR transport in infrastructure. Chat orchestration consumes text through the application layer. Verify the pinned ASR server's API before choosing a client; a model name alone does not establish endpoint compatibility.

## Validation

```sh
cargo test --locked
cargo clippy --all-targets --all-features --locked -- -D warnings
```

Add mocked transcription tests, encrypted media coverage, authorization and resource-limit tests, plus a manual Matrix voice-message test against the homelab model.

## Progress

- 2026-10-24 — Created at the user's request for voice chat with Pan; interpreted “Iser” as ASR and selected the existing local `qwen3-asr` model as the initial target.

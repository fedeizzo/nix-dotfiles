# Open WebUI TODO

Features to explore (Open WebUI 0.11.4).

## Integrations

- [ ] **Tools / MCP**: connect MCP or OpenAPI tool servers so models can call homelab services (Home Assistant, Paperless, Outline, Nextcloud).
- [ ] **Calendar**: built-in calendars and events (RSVP, sharing), readable by models. No CalDAV/Google sync upstream: bridge the Nextcloud calendar through a tool or MCP server.
- [ ] **Automations**: scheduled prompts (RRULE), e.g. a morning brief using web search. Runs show in the "Scheduled Tasks" calendar.

## Models and knowledge

- [ ] **Custom models**: base model + system prompt + knowledge + tools as a picker entry.
- [ ] **Knowledge**: document collections for RAG (embedded with `embeddinggemma-2`), attached with `#`.
- [ ] **Skills**: reusable instruction packs models load on demand.
- [ ] **Memories**: facts about the user kept across chats.
- [ ] Hide non-chat models from the picker (`embeddinggemma-2`, ASR/TTS, image, duplicated `forge/*` peers).

## Workspace

- [ ] **Notes**: built-in notes editor with AI assist, usable as chat context.
- [ ] **Folders / projects**: chats grouped with their own system prompt and files.
- [ ] **Code interpreter**: Python in the browser (Pyodide) for charts and data work.
- [ ] **Channels**: shared rooms for people and models (`ENABLE_CHANNELS`, off by default).

## Blocked / follow-ups

- [ ] **Voice / call mode**: blocked on gufo STT upload limits, https://github.com/gufo-org/gufo/issues/1418.
- [ ] **Image generation**: after deploy, run `systemctl start gufo-model-sync` and enable it in Admin → Settings → Images (DB already seeded, env is ignored).
- [ ] Fail evaluation when the model IDs in `open-webui.n.nix` (task, embedding, STT, TTS, image) are missing from `services.llama-swap.settings.models`.

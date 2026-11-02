# model-store

Layout and sync tooling for `/persist/models` (shared by all model hosts).

## Layout contract

```
/persist/models/
├── store/<Family-Size[-Variant]>/   # one dir per model, all quants inside (flat, never grouped)
├── by-capability/<cap>/             # GENERATED relative symlinks only — never real data
└── samples/                         # loose audio/text samples
```

Plain `.gguf` quants live directly in the model dir; GGUF checkpoints that need
config scaffolding get a `gguf/` subdir. Never let HF caches
(`models--*--*`, `blobs/`, `CACHEDIR.TAG`) appear here — download with
`HF_HUB_CACHE` set outside the store, then move the finished file in.

## Syncing hosts

```bash
./sync-models.sh [user@host]     # default root@192.168.1.89 (forge)
```

Union merge (`rsync -au`, newest wins), then regenerates `by-capability/` on
both hosts. Run after downloading a model on either machine. Deletions are NOT
propagated — remove retired models on both hosts by hand.

New models: add them to the `MEMBERS` map in `sync-models.sh` to include them
in a capability category; a missing entry only warns, never fails.

# Codebase Concerns

## Core Sections (Required)

### 1) Top Risks (Prioritized)

| Severity | Concern | Evidence | Impact | Suggested action |
|----------|---------|----------|--------|------------------|
| High | Agent Mutation Safety | `plan.md` | `pan optimize` running in loop could accidentally perform real mutative API calls if `dry_run` is not properly checked by the tool implementation. | Ensure every mutative tool has `context.WithValue(ctx, "dry_run", true)` checks implemented securely. |
| Med | Matrix / Bot State Storage | `go.mod` (sqlite3) | Using local SQLite for bot state or agent sessions may limit horizontal scaling of the `pan` service. | If multi-instance is needed, migrate memory storage to PostgreSQL. |

### 2) Technical Debt

| Debt item | Why it exists | Where | Risk if ignored | Suggested fix |
|-----------|---------------|-------|-----------------|---------------|
| Unknown E2E Testing | Needs manual bot execution | `internal/bot` | Breaking changes to Matrix API integration go unnoticed. | Implement mock Matrix homeserver tests. |

### 3) Security Concerns

| Risk | OWASP category (if applicable) | Evidence | Current mitigation | Gap |
|------|--------------------------------|----------|--------------------|-----|
| Insecure credential execution | A02:Cryptographic Failures | `config.go` (`exec.Command`) | Credentials can be sourced by running arbitrary shell commands `*_CMD`. | Commands run with `sh -c`. Ensure these fields are heavily validated or restricted by the Nix OS deployment environment to prevent injection. |

### 4) Performance and Scaling Concerns

| Concern | Evidence | Current symptom | Scaling risk | Suggested improvement |
|---------|----------|-----------------|-------------|-----------------------|
| LLM API Latency | `config.go` | Bot could block processing multiple events simultaneously. | Backpressure on Matrix messages. | Ensure bot event processing leverages goroutines and `samber/ro` streams efficiently. |

### 5) Fragile/High-Churn Areas

| Area | Why fragile | Churn signal | Safe change strategy |
|------|-------------|-------------|----------------------|
| `go.mod` / `go.sum` | Dependencies updated frequently | 4 commits in last 90 days. | Ensure tests cover integration points with newer modules. |
| `Justfile` / `*.nix` | Deployment config tweaks | High churn across homelab hosts | Apply changes via Nix flakes and verify locally first. |

### 6) `[ASK USER]` Questions

None remaining (Resolved: rate limits are not a concern for the optimizer loop, Matrix lacks E2E tests by design, Fusion is an RSS reader, and there are no target coverage thresholds).

### 7) Evidence

- `plan.md`
- `internal/config/config.go`
- `docs/codebase/.codebase-scan.txt`

---
id: PAN-008
title: Integrate Pi as an isolated development worker
status: ready
priority: P1
owner: unassigned
created: 2026-10-23
updated: 2026-10-23
labels:
  - development
  - pi
  - matrix
  - security
depends_on: []
related: []
---

# PAN-008: Integrate Pi as an isolated development worker

## Summary

Let the authorized user request features through Matrix and delegate implementation to Pi. Return a reviewable patch and validation results without rebuilding a coding loop inside Pan.

## Context

Pan currently handles read-only assistance and deterministic email and finance workflows. It cannot edit repositories. Pi supports a language-independent [RPC interface](https://pi.dev/docs/latest/rpc) suitable for Rust integration. Pan's worker must have its own configuration owned by the Pan service module, independent of the personal Pi setup under `modules/dev/pi/`, including its `config/` directory.

The initial target is Pan's own source. Changes must be prepared in a separate checkout while the running service remains on its deployed version.

## Scope

- Add an explicit Matrix development command for an allowlisted repository, starting with Pan.
- Record the request, scope, acceptance criteria, owner, conversation binding, job state, and workspace location durably.
- Implement a domain/application worker port and a Pi RPC subprocess adapter; keep protocol details outside Matrix handlers.
- Run each job in a separate checkout and an enforced isolated environment with bounded runtime and resources.
- Give workers a dedicated identity, minimal pinned Pi configuration, explicit llama-swap model, and only necessary development tools and network access.
- Maintain a dedicated, declaratively managed worker configuration alongside Pan. Configure its instructions, behavioral defaults, allowed tools, reviewed extensions/skills, model/provider routing, and resource limits independently of personal Pi settings.
- Control Pi's configuration/resource discovery and environment so personal settings and repository-local configuration cannot silently enable extensions, providers, tools, or hooks outside the approved worker policy.
- Forward summarized progress, follow-up instructions, clarification requests, status queries, and cancellation through the originating Matrix thread.
- Return the resulting diff, changed files, test commands/results, and unresolved issues for human review.
- Add NixOS configuration and operator documentation for the worker environment and retained artifacts.

## Out of Scope

- Reimplementing Pi's coding loop, tools, or context management.
- Automatic merging, pushing, PR publication, deployment, or modification of the running service.
- Arbitrary repository URLs or unrestricted host shell access.
- Giving development workers access to email, financial records, Pan service credentials, or personal Pi credentials/extensions.

## Acceptance Criteria

- [ ] An authorized Matrix request creates exactly one durable development job and isolated checkout for an allowlisted repository; unauthorized requests are rejected.
- [ ] Pi can implement a small Pan feature and run relevant checks in that checkout without changing the deployed source or a user's working copy.
- [ ] Worker isolation prevents reading Pan's service secrets and personal home, writing outside approved workspace/state paths, accessing host control sockets, or obtaining deployment privileges.
- [ ] Worker configuration pins Pi and reviewed extensions, selects the intended local model explicitly, and does not inherit personal provider credentials or external-model routing.
- [ ] Changing `modules/dev/pi/` or a user's Pi settings has no effect on Pan workers; the worker does not import, symlink, or copy personal Pi configuration.
- [ ] Operators can independently change worker instructions and supported behavior/tool restrictions through the Pan worker configuration; effective settings and precedence are documented.
- [ ] Repository-local Pi configuration and instructions cannot override enforced worker permissions or activate unapproved executable resources. Tests cover conflicting personal and project settings.
- [ ] RPC framing, command correlation, progress, clarification, completion, and cancellation match the pinned Pi version; prompt acceptance is never mistaken for job completion.
- [ ] Job state and thread associations survive Pan restarts; an interrupted worker is reconciled or reported for explicit retry without silently launching duplicate work.
- [ ] Cancellation and timeouts terminate the worker and its child processes; malformed output and unexpected exits produce actionable job failures.
- [ ] Concurrent jobs have distinct workspaces and sessions, and configured concurrency/resource limits are enforced.
- [ ] The user receives a diff or patch and validation evidence; publishing, merging, and deployment are unavailable in this initial workflow.
- [ ] Logs and Matrix progress omit credentials and unnecessary source content; retention and cleanup behavior are documented.
- [ ] README and operator documentation explain commands, configuration, isolation, recovery, and limitations.

## Implementation Notes

Use Pi RPC rather than its Node SDK. Check the pinned version's protocol before implementing; current documentation distinguishes prompt acceptance from final `agent_settled` completion. Continuously drain stdout and stderr, bound record sizes, and pass commands as structured JSON rather than shell interpolation.

Treat repository instructions, extensions, build scripts, and model output as untrusted. A subprocess or separate checkout alone is not a sandbox. Enforce permissions outside the model, and do not relax Pan's existing service hardening to grant it development access.

Use a dedicated Pi configuration/state location and a sanitized environment. Verify the pinned Pi release's discovery and override mechanisms rather than assuming a custom working directory prevents loading user or project resources. Separate editable behavioral guidance from security controls enforced by the worker launcher and sandbox. Worker configuration must not be writable by coding jobs; proposals to change that policy follow human review and the deployment approval boundary.

Use one Matrix thread per job and a persistent Pi session where appropriate. Keep job lifecycle separate from Pan's short assistant-call timeout. Preserve the repository's Jujutsu workflow. Future approval-to-publish or deployment features require separate tasks.

## Validation

```sh
cargo fmt --check
CARGO_BUILD_JOBS=2 cargo test --locked
CARGO_BUILD_JOBS=2 cargo clippy --locked --all-targets --all-features -- -D warnings
```

Add fake-Pi protocol tests covering split records, malformed/oversized records, acceptance versus completion, follow-ups, stderr backpressure, crashes, cancellation, and restart recovery. Test authorization and duplicate Matrix delivery at the application boundary.

Add a NixOS worker integration test proving filesystem/secret isolation and process cleanup. Evaluate and build the affected NixOS configuration. Perform a controlled live Pi smoke test against a disposable fixture repository with no production credentials; verify patch output and independent test results.

Place conflicting settings and sentinel extensions in fixture personal/project Pi directories and verify they are not loaded. Verify changes to the dedicated configuration affect worker behavior while disallowed resources remain blocked.

## Progress

- 2026-10-23 — Created at the user's request after removing PAN-001 through PAN-007. Pi will provide the coding loop; Pan will own authorization, durable jobs, isolation, and review delivery.
- 2026-10-23 — Clarified that Pan owns a dedicated Pi configuration independent of `modules/dev/pi/`. Added configurable behavior, controlled resource discovery, immutable worker policy, and tests for conflicting personal/project settings.

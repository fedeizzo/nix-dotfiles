---
id: PAN-012
title: Evaluate and integrate OpenShell for development jobs
status: ready
priority: P1
owner: unassigned
created: 2026-10-24
updated: 2026-10-24
labels:
  - development
  - openshell
  - security
  - nixos
depends_on: []
related:
  - PAN-008
  - PAN-010
---

# PAN-012: Evaluate and integrate OpenShell for development jobs

## Summary

Evaluate NVIDIA OpenShell as the sandbox for Pi development jobs on homelab. Replace the current Bubblewrap launcher and model bridge only after verifying NixOS compatibility, isolation, and the complete Pan workflow.

## Context

Pan currently uses [Bubblewrap launchers](../worker.n.nix) with a dedicated Pi configuration, disposable repository checkouts, and a local llama-swap connection. The user confirmed a real Matrix development request successfully produced a change after resolving a systemd procfs masking conflict.

OpenShell was discussed as a future foundation for controlling filesystem access, network access, and credentials. No OpenShell integration exists yet. This work is a priority for continuing the project and should establish the sandbox boundary before expanding worker network access or introducing publishing credentials.

Related work: [Pi worker](PAN-008-pi-development-worker.md) and [reviewed PR publication and deployment](PAN-010-signed-comin-deployments.md).

## Scope

- Inspect the current upstream architecture, Pi integration, runtime requirements, policy semantics, and supported environments using the official [repository](https://github.com/NVIDIA/OpenShell) and [documentation](https://docs.nvidia.com/openshell/latest/).
- Build a proof of concept on NixOS: Pi edits a disposable checkout using local llama-swap and Qwen 27B, without host credentials.
- Establish the threat model: which components are trusted, where policy is enforced, what access the agent has, and what happens if Pi or repository content is malicious.
- Determine and pin the runtime and packaging approach; document required kernel features, privileges, networking, persistence, upgrades, and recovery.
- Define explicit filesystem and outbound network policy, including repository fetches and model access. Test both allowed and denied operations.
- Adapt the infrastructure executor while preserving Pan's domain/application boundary, dedicated Pi configuration, durable queue, Unix control socket, Matrix authorization, cancellation, deadlines, and review artifacts.
- Ensure Pi cannot change the effective policy or load personal/project configuration that grants additional authority.
- Migrate the homelab worker only after the evaluation passes. Remove replaced Bubblewrap/model-bridge components once OpenShell provides their required behavior.

## Out of Scope

- Giving the worker trusted signing keys, merge authority, root access, or deployment authority.
- Adding GitHub credentials, dependency-download access, PR publication, or automatic deployment as part of the initial migration.
- Moving Pan's email and finance integrations into the coding sandbox.

## Acceptance Criteria

- [ ] A documented evaluation records the pinned version, NixOS runtime requirements, threat model, tradeoffs, and an explicit adoption decision. Unmet security or compatibility requirements leave the existing worker in place with concrete follow-up work recorded.
- [ ] A real Pi request through Pan uses the local model, edits only its job checkout, and returns a summary and patch to the originating Matrix conversation.
- [ ] Tests demonstrate denied access to host homes, secrets, SSH/signing agents, Nix daemon, unrelated job directories, and unauthorized network destinations, including paths through local services.
- [ ] The agent cannot modify or bypass the enforced policy; dedicated Pi settings remain independent of personal and repository configuration.
- [ ] Prompt acceptance, completion detection, bounded diagnostics, failed model calls, cancellation, timeouts, and worker restarts behave correctly; termination leaves no job processes or leaked resources.
- [ ] NixOS deployment declares runtime dependencies and required state, works after reboot, and documents rollback to the previous worker.
- [ ] Existing worker tests remain applicable or have equivalent coverage; deployment documentation explains the resulting isolation and its limits.
- [ ] If adopted, obsolete Bubblewrap launchers and the former model bridge are removed after parity is verified.

## Implementation Notes

Keep Pi as the coding loop and Pan as the authorization, orchestration, persistence, and notification layer. OpenShell belongs behind the development executor interface. Do not assume a Pi tutorial proves RPC compatibility or that a development flake supplies a production NixOS module; verify these during evaluation.

This card can begin independently of the remaining PAN-008 work. Coordinate shared executor changes and complete the sandbox evaluation before expanding the worker's access in PAN-010.

## Validation

From `modules/services/apps/pan`:

```sh
cargo test --locked
cargo clippy --all-targets --all-features --locked -- -D warnings
cargo fmt --check
```

Evaluate and build the homelab NixOS configuration. Run runtime isolation tests on NixOS covering positive and negative policy cases, cancellation/process cleanup, restart recovery, and model access. Finish with an operator-observed Matrix development request and a reboot test; record exact commands and results in this card.

## Progress

- 2026-10-24 — Created at the user's request as a P1 next step. Bubblewrap currently works on homelab; OpenShell remains an evaluation followed by a conditional migration.

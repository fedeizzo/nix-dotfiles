---
id: PAN-010
title: Deploy reviewed infrastructure changes through signed Comin commits
status: backlog
priority: P1
owner: unassigned
created: 2026-10-23
updated: 2026-10-23
labels:
  - deployment
  - comin
  - security
  - github
depends_on:
  - PAN-008
related: []
---

# PAN-010: Deploy reviewed infrastructure changes through signed Comin commits

## Summary

Let Pan delegate infrastructure changes to Pi, publish draft PRs, and report deployment results. After human review and signing of the final deployment commit, Comin verifies and applies the homelab configuration.

## Context

[PAN-008](PAN-008-pi-development-worker.md) prepares isolated development jobs but excludes PR publication and deployment. This card adds those capabilities without giving Pan or Pi deployment authority.

The homelab currently uses [deploy-rs](../../../../system/nix/flake/deployments.nix) with rollback enabled. [Comin](https://github.com/nlewo/comin) offers pull-based deployment and verification against locally configured signing keys. GitHub account compromise must not, by itself, authorize a new system configuration.

## Scope

- Extend development jobs to an allowlisted infrastructure repository and publish feature branches and draft PRs using a restricted publishing identity.
- Support an explicit update-nixpkgs request; define which nixpkgs input it updates, preserve unrelated lock entries, and report changed packages and build results.
- Add a Comin NixOS module following repository conventions, targeting `nixosConfigurations.homelab`.
- Choose and document the protected deployment branch and promotion workflow: human review followed by locally signing the exact final deployment commit, using Jujutsu where supported.
- Pin trusted deployment public keys on the server; keep private keys and signing agents inaccessible to Pan, Pi, CI, and the homelab. Prefer a hardware-backed signing key requiring human presence.
- Disable Comin testing branches and avoid worker-writable local remotes or alternate deployment paths that bypass signature verification.
- Persist Comin state under the homelab's impermanence setup and define retention, bootstrap, key rotation, revocation, and recovery procedures.
- Define health checks, failure notifications, and recovery before replacing deploy-rs as the normal deployment path. Retain a documented manual recovery route and prevent competing deployment mechanisms from running concurrently.
- Report deployment commit, status, and health through independent monitoring/notifications that continue while Pan restarts.

## Out of Scope

- Automatic human approval, signing, merging, or granting the worker a trusted deployment key.
- Deploying draft PRs, arbitrary branches, or unsigned revisions.
- Treating signatures as proof of code safety or promising that rollback reverses leaked data or database migrations.
- Arbitrary imperative root commands requested through chat.

## Acceptance Criteria

- [ ] An authorized request produces a scoped branch and draft PR with a diff, validation results, and a clear link to the originating Pan job.
- [ ] Publishing credentials cannot directly update the protected deployment branch, merge PRs, alter branch protections, or obtain deployment/signing credentials; document required GitHub rules and permissions.
- [ ] Comin deploys only the configured branch and host after verifying the final commit against explicitly pinned public keys, before evaluating candidate Nix code.
- [ ] Unsigned commits, commits signed by an unknown key, and GitHub web-signed commits are rejected even if pushed to the deployment branch.
- [ ] An attacker-controlled candidate cannot change the active trusted-key policy to authorize itself.
- [ ] Testing branches and worker-writable remotes cannot trigger activation; Pan/Pi cannot bypass the signed promotion boundary.
- [ ] The signing procedure binds approval to the exact reviewed deployment revision and explains why a GitHub merge click alone may not produce an acceptable signature.
- [ ] Comin state survives reboot and persistence restoration; replay/rollback behavior is tested against the pinned Comin version and documented.
- [ ] Build/evaluation failures preserve the current running configuration; activation and post-deployment health failures produce alerts and have a tested recovery procedure, including lost network access.
- [ ] Successful deployment of a Pan change restarts the intended service, preserves pending job state, and reports the deployed revision independently of Pan.
- [ ] Private repository credentials use SOPS-managed files and read-only Comin access; logs and notifications omit secrets.
- [ ] Key rotation/revocation, deployment pause, bootstrap, manual recovery, and limits of signatures/hash-pinned dependencies are documented.

## Implementation Notes

Use the pinned Comin version's actual signature and update behavior. Do not trust GitHub's generic web signing key or dynamically fetch trusted signing keys from a GitHub account. Validate that signature checks occur before evaluation and that policy comes from the running trusted deployment.

A signed deployment branch can be separate from the PR merge branch if that makes human promotion clearer. Decide this during implementation and record the decision. Signing must occur on a trusted human-controlled device; do not expose its signing agent to repository build scripts.

Comin deploys a NixOS system configuration, so a Pan-only change still follows the system deployment path. Define service and network health separately from successful activation. Review the existing container-restart timer before enabling unattended switches.

## Validation

```sh
cargo fmt --check
cargo test --locked
cargo clippy --locked --all-targets --all-features -- -D warnings
# From the repository root:
nix build .#nixosConfigurations.homelab.config.system.build.toplevel --no-link
```

Add application tests for authorized publication, duplicate delivery, retry/recovery, and scoped input updates. Use a disposable remote and NixOS VM to verify trusted/unknown/unsigned/web-signed commits, candidate trust-policy changes, disabled testing branches, persisted state, successful deployment, failed builds, and activation/recovery behavior. Keep production credentials and signing keys out of fixtures.

Perform a controlled end-to-end trial: Matrix request → Pi patch → draft PR → review → human-signed promotion → Comin deployment → independent health report. Record exact test commands and results before closing the card.

## Progress

- 2026-10-23 — Created at the user's request. Deployment approval requires a trusted human signature independent of GitHub account access; implementation follows PAN-008.

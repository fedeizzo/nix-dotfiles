# forge — ideas

`forge` is the second Framework Desktop (Strix Halo, 128 GB). It shares its base with
`homelab` through dendritic feature modules:

| Feature | File | What it brings |
|---------|------|----------------|
| `strix-halo` | `modules/system/hardware/strix-halo.n.nix` | nixos-hardware, kernel params (GTT 124 GiB), ROCm, tuned |
| `ephemeral-root` | `modules/system/storage/ephemeral-root.n.nix` | disko LUKS + btrfs, root wiped on boot, base persistence |
| `server` | `modules/system/server.n.nix` | root access, hardened sshd, CLI tools, nix gc |
| `mixer` | `modules/system/user/mixer.n.nix` | `mixer` user + home-manager (homelab only; forge is root-only) |

Right now forge only runs `llama-swap` with `qwen3.8-27b`. homelab keeps every model and OpenRouter, and
reaches forge's copy as the llama-swap peer model `forge/qwen3.8-27b`. Below are ways to make the two machines work as one.

## 1. Split services
forge takes the AI work: llama-swap, lemonade, the voxtype/whisper backends, and immich ML.
homelab keeps the stateful apps (postgres, nextcloud, paperless, authentik). Traefik on homelab stays
the single entry point and proxies AI subdomains to `forge:11435`. Each box then has all of its memory
for its own job.

## 2. Combined 256 GB for LLMs (llama.cpp RPC)
forge runs `rpc-server`, and llama-swap on homelab adds `--rpc forge:50052` to large models so their
weights span both machines (for example, big MoE models at Q4). Ethernet is the bottleneck: connect the
two machines directly over USB4 (thunderbolt-net, ~40 Gbps) and configure that link in a dedicated
feature module.

## 3. Shared builds and cache
- Each host is a `nix.buildMachines` entry for the other (`nix.distributedBuilds = true`).
- One host serves its store to the other with `harmonia`, so ROCm/llama.cpp builds happen once.

## 4. Backup and failover between the two
- homelab restic (or `btrfs send` snapshots) also targets forge: a fast local copy besides Backblaze B2.
- A second blocky instance on forge keeps DNS up while homelab reboots.
- Each host's gatus monitors the other.

## 5. One network map
- Give forge a WireGuard peer (`192.168.7.x`) so it's reachable from the laptops.
- Add `topology.self` for forge in nix-topology.

## 6. Boot hardening (TPM)
LUKS is unlocked by the TPM, bound to PCR 7. While Secure Boot is off, PCR 7 doesn't distinguish this
system from any other OS booted on the machine, so TPM unlock gives little protection against someone
with physical access. Next step: lanzaboote (Secure Boot), then re-enroll bound to PCR 7 (+ 11 / signed
PCR policy). The PiKVM covers passphrase entry when the TPM refuses.

## 7. BIOS
Set the iGPU UMA frame buffer to the minimum (as on homelab). The kernel's `amdgpu.gttsize` already
lets the GPU use up to 124 GiB dynamically; a 64 GB carve-out only hides that memory from the OS.

## 8. Harden collie on forge
collie (`collief.fedeizzo.dev`) listens on forge's LAN address, and only homelab is allowed through the
firewall. It has no auth of its own, so anything on the LAN that spoofs homelab's IP can reach it.
- Set `COLLIE_TOKEN` (from sops) so the bridge itself checks requests.
- Or keep collie on loopback and have homelab reach it over an SSH tunnel (`ssh -L 8787:localhost:8787 forge`
  as a systemd service), then drop the firewall rule.

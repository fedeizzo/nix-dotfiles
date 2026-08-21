# Framework Desktop / Homelab Security Review

## Executive summary

This document records the results of the static security review defined in `security_plan.md`.

The reviewed system is the `nixosConfigurations.homelab` configuration composed from `modules/hosts/framework-desktop/`. The review followed its active local imports, host fragments, relevant Home Manager modules, service definitions, flake inputs, network policy, identity configuration, secrets handling, storage, and backups.

### Overall assessment

The configuration has several good foundations—Nix flake locking, an enabled host firewall, LUKS-encrypted primary storage, SOPS-managed service secrets, disabled SSH password authentication, and some well-sandboxed services—but its effective trust boundaries are substantially broader than the reverse-proxy design suggests.

The highest-risk themes are:

1. `wg0` is a fully trusted firewall interface, so every VPN peer can reach every host listener rather than only the explicitly listed ports.
2. Several services bypass Traefik segmentation through host networking, wildcard binds, LAN firewall exceptions, or Docker-published ports.
3. The interactive `mixer` account is root-equivalent through Docker, wheel/doas, and Nix trusted-user privileges; direct root SSH and credential/key reuse increase the blast radius.
4. Password verifiers and a Grafana application secret are committed as ordinary Nix strings.
5. Active OCI workloads use mutable image tags, so the deployed container contents are not fixed by `flake.lock`.
6. A compromised primary host appears able to destroy both its local backup and its writable remote backup history.

No critical-severity issue was proven using static evidence alone. After independent review and severity normalization, this report contains **four High**, **thirteen Medium**, **one Low**, and **one Informational** finding. Runtime-dependent claims are labeled probable or needs validation rather than counted as confirmed exposure.

## Review boundaries

### Performed

- Read-only inspection of repository configuration
- Tracing of the effective framework-desktop/homelab import graph
- Static inspection of firewall, WireGuard, SSH, users, privilege, containers, reverse proxy, secrets, boot/storage, backups, and systemd services
- Cross-correlation into plausible attack paths

### Not performed

- Nix evaluation, build, or activation
- Runtime listener/firewall inspection
- Port scanning or service probing
- Secret decryption or password cracking
- Inspection of router/NAT, DNS provider, Backblaze account policy, firmware state, or live file permissions
- Exhaustive audit of the internals of third-party NixOS modules
- Git-history or remote-mirror secret scanning

A finding marked **Confirmed statically** is directly represented by active local configuration. **Probable** means the declaration is clear but final behavior depends on generated/runtime state. **Needs runtime validation** is not presented as a confirmed vulnerability.

## Effective configuration map

```text
flake.nix
└─ flake-parts.mkFlake + import-tree ./modules
   └─ modules/dendritic.nix
      └─ nixosConfigurations.homelab
         ├─ hostname = "homelab"
         ├─ username = "homelab"
         └─ self.modules.nixos.framework-desktop
            ├─ Explicit imports in configuration.n.nix
            │  ├─ external: nix-topology, authentik-nix, hermes-agent,
            │  │  home-manager
            │  ├─ base: misc, nix, security, disk, power, hardware, boot,
            │  │  networking, wireguard, sops, environment, persistence
            │  └─ services: every service named at configuration.n.nix:53-88
            └─ import-tree host fragments merged into the same module
               ├─ system/backup.n.nix
               ├─ system/boot.n.nix
               ├─ system/deployment.n.nix
               ├─ system/disko.n.nix → external Disko import
               ├─ system/hardware.n.nix → external nixos-hardware import
               ├─ system/misc.n.nix
               ├─ system/networking.n.nix
               ├─ system/nix.n.nix
               ├─ system/persistent.n.nix
               ├─ system/sops.n.nix
               ├─ system/topology.n.nix
               ├─ system/users.n.nix
               ├─ system/wireguard.n.nix
               └─ users/mixer.n.nix
                  ├─ imports local nixos.ai-tools
                  └─ composes the mixer Home Manager modules
```

Primary evidence:

- `flake.nix:5-7`
- `modules/dendritic.nix:4-13`
- `modules/hosts/framework-desktop/configuration.n.nix:14-103`
- `modules/hosts/framework-desktop/system/hardware.n.nix:2-10`
- `modules/hosts/framework-desktop/system/disko.n.nix:2-12`
- `modules/hosts/framework-desktop/users/mixer.n.nix:2-65`

The directory name is not the configuration name: the evaluated host entry is `nixosConfigurations.homelab`. The second map branch is important: host fragment files are not ordinary entries in `configuration.n.nix`'s `imports` list; `import-tree` discovers their declarations and merges them into the same `flake.modules.nixos.framework-desktop` attribute that the host imports. Other modules merely discovered elsewhere in the tree are not assumed active unless reached by these explicit imports or merged declarations.

## Attack-surface inventory

### Principal trust zones

| Zone | Intended boundary | Static observation |
|---|---|---|
| Internet | Traefik `websecure` and explicitly exposed services | Router/NAT is unknown. Affine is marked exposed and uses host networking. |
| Ethernet LAN | TCP/443 and UDP/53 plus selected direct services | Calibre/Bookbridge ports are explicitly opened. Address-less Docker publications may add direct paths. |
| WireGuard | Intended private service/admin access | `wg0` is trusted, effectively allowing every host listener to every peer. |
| Host loopback | Intended backend-only services | Host-network containers can reach this namespace, weakening loopback as a boundary. |
| Containers | Intended application isolation | Several workloads use host networking, writable host mounts, mutable tags, and no explicit privilege reduction. |
| Local interactive user | Human/AI/development account | `mixer` is root-equivalent through several independent mechanisms. |
| Backup plane | Recovery after host compromise | Local and remote backup destruction appear possible from the primary host. |

### Important declarative exposure

| Service/path | Declarative exposure | Authentication/boundary note |
|---|---|---|
| Traefik HTTPS | `eth0:443` and WG entry point | Public/private routing is generated from `fi.services`; not every internal route adds Authentik. |
| Blocky DNS | `eth0` UDP/53 | LAN-wide; bind and recursion ACL behavior need runtime evaluation. |
| WireGuard endpoint | Host-wide UDP/51821 from the shared WireGuard module | Intended VPN ingress; upstream router exposure is external to the repo. |
| SSH | Enabled; root login allowed by key | Firewall not globally opened, but trusted `wg0` makes it reachable from every VPN peer. |
| Calibre | `0.0.0.0:44533`, opened on `eth0` | Direct LAN path bypasses Traefik's WG-only routing choice. |
| Bookbridge/KOSync | host network; TCP/52914 opened on `eth0` | Direct LAN exposure and host-network pivot potential. |
| Affine | exposed through Traefik; `--network=host` | Internet-routed application shares host network and receives a secret environment file. |
| Fusion | Docker publication `51000:8080` | No host address; probably all-interface publication. |
| epub2audiobook | Docker publication `7860:7860` | Application also binds `0.0.0.0`; final reachability needs runtime validation. |
| Flaresolverr | Docker publication `8191:8191` | Mutable image and no declared resource limit. |
| Jellyfin | `services.jellyfin.openFirewall = true`; also public Traefik registration | Generated firewall ports require Nix evaluation; direct service exposure can bypass proxy-only assumptions. |
| Home Assistant HTTP | `openFirewall = true`, but HTTP binds to IPv6 loopback `::1:8123` | A generated firewall declaration exists, but direct HTTP reachability is not established because the configured listener is loopback-only. |
| Traefik API/dashboard | `api.insecure = true` | Unprotected API conventionally listens on its default port; exact evaluated port needs validation. |
| Traefik metrics | `:8082` | Wildcard listener reachable from trusted VPN unless another control intervenes. |
| Backrest | `0.0.0.0:9898`, root service | Registered for proxy auth, but direct trusted-interface reachability and native auth need validation. |
| Neo4j | WG ports 7474/7687 listed | WG trust makes these explicit allowlists non-restrictive. |
| Mosquitto MQTT | Listener declared with anonymous access and `readwrite #` | Bind address/port reachability requires evaluation; a wildcard default would make it reachable from trusted WG peers. |
| GarminDB container | Active host networking | Shares the host namespace to reach InfluxDB; not Internet-marked, but reachable trust depends on its own listener behavior. |
| Paperless helper containers | Host networking, `autoStart = false` | Not assumed active at boot; if manually started they join the flat host/WG attack surface. |
| Hermes container declaration | Host networking in its service module | The local `self.modules.nixos.hermes` import is commented out; the external Hermes module remains active, so this local container is not treated as proven active. |
| AI/media services | Several wildcard/host listeners | Resource exhaustion and lateral movement are credible from trusted or compromised service paths. |

### Identity and privilege inventory

| Identity/workload | Effective authority |
|---|---|
| `root` | Local password verifier, SSH authorized key, deployment login, all host/SOPS/backup authority. |
| `mixer` | Interactive user; member of `wheel` and `docker`; wheel is allowed through doas and trusted by Nix; imports AI/development tooling. |
| `nixremote` | Listed as a Nix trusted user in `modules/system/nix/nix.n.nix`; no declarative account was found in the reviewed active tree, so existence and access are runtime-validation items. |
| Restic backup jobs | Run as root and consume runtime SOPS repository/password files. |
| Backrest | Runs as root and manages root-owned persistent configuration/data. |
| Docker containers | Docker daemon is rootful; container escape or Docker-socket access is therefore host-critical. |
| Service identities | Authentik, Gatus, GarminDB, ntfy, Nextcloud, Immich, Traefik, Open WebUI, n8n, Pan, Paperless and others declare dedicated or module-managed users; effective IDs and permissions require evaluation. |

### Secrets boundary inventory

- A single host Age identity at `/var/lib/sops/keys.txt` decrypts the homelab recipient set selected by `.sops.yaml`.
- The WireGuard private key, database passwords, application secrets, OAuth credentials, API tokens, backup repository passwords, and backup provider credentials are generally consumed through `config.sops.secrets.<name>.path`.
- Consumers use `passwordFile`, `environmentFile(s)`, or application-specific file options. Several declarations specify `0400`/`0440` and service ownership; effective runtime modes remain a validation item.
- Traefik additionally reads `/var/container_envs/traefik`, which is persisted and backed up. Its provisioning, ownership, mode, content source, and rotation are not declared in the reviewed Traefik module and require runtime validation.
- Root compromise reaches the Age identity and therefore has a broad decryption/rotation blast radius.
- Two password verifiers and the Grafana application key are inline repository/Nix exceptions and are findings below; the Traefik environment file is a separate undeclared-provisioning exception.

### Supply-chain inventory

- `flake.lock` pins Nix flake inputs at a commit, but updates to it are privileged code-review events.
- Active external modules include nix-topology, authentik-nix, hermes-agent, Home Manager, Disko, nixos-hardware, SOPS-Nix, and service-specific external modules reached by imports.
- `pkgs-unstable` and selected overlays are available; `nixos.ai-tools` actively applies the llm-agents overlay.
- Runtime OCI images are fetched from Docker Hub/GHCR and several use mutable tags, so they are not reproducible from `flake.lock` alone.
- Blocky downloads DNS blocklists from mutable `latest`/`main` URLs on a four-hour schedule (`modules/services/infra/blocky/blocky.n.nix:21-33`).
- Llama-swap references Hugging Face models without immutable revision pins (`modules/services/ai-workflows/llama-swap/llama-swap.n.nix:85-107`).
- The host permits insecure .NET 6 package identifiers (`modules/hosts/framework-desktop/system/nix.n.nix:4-10`); permission is confirmed, but actual installation in the evaluated closure was not established.

## Positive controls observed

These controls materially reduce risk and should be preserved while remediating findings:

- The NixOS firewall is enabled, and the host-specific networking fragment sets an empty base `allowedTCPPorts` list.
- This is only a base policy, not the final effective port set: active Jellyfin and Home Assistant modules set `openFirewall = true`, the WireGuard module opens UDP/51821, and service/interface/Docker rules add other paths. Home Assistant HTTP itself is configured for IPv6 loopback, so an open-firewall declaration does not by itself prove a reachable HTTP listener. Generated effective ports require evaluation.
- Some Ethernet exposure is constrained through explicit interface rules.
- SSH password and keyboard-interactive authentication are disabled.
- Primary storage outside the ESP is LUKS encrypted.
- Service credentials are generally managed through SOPS runtime files rather than interpolated plaintext.
- Tracked homelab secret payloads inspected by the review appeared to contain SOPS ciphertext markers; no values were decrypted.
- Several SOPS secrets declare restrictive modes and service-specific ownership.
- The repository has a `flake.lock`, so flake inputs are reproducible at a given commit.
- Traefik disables update checking and anonymous usage reporting.
- Grafana enables secure/strict cookies, disables basic auth and self-registration, and uses SOPS files for OAuth credentials.
- Some custom services, such as Pan and GarminDB, declare useful systemd sandboxing controls.
- Restic performs frequent local backups, uses encrypted repositories, and has a remote Backblaze target.
- PostgreSQL logical dumps are enabled in addition to raw data paths.
- Fail2ban configuration exists for SSH/Traefik-related protection.

These controls do not cancel the findings below; for example, VPN interface trust bypasses narrow per-port firewall intent, and encrypted Restic repositories remain deletable by a client with write/delete credentials.

# Findings

## F-01 — WireGuard is a flat trusted zone and LAN pivot

- **Severity:** High
- **Status:** Confirmed statically
- **Evidence:**
  - `modules/hosts/framework-desktop/system/networking.n.nix:20-33` enables the firewall but places `wg0` in `trustedInterfaces`.
  - `modules/hosts/framework-desktop/system/wireguard.n.nix:20-38` forwards unrestricted traffic from `wg0` to `192.168.1.254` and masquerades it.
  - Multiple peers are declared later in the same WireGuard module.
- **Why it matters:** The explicitly listed WG ports are not a meaningful boundary once the whole interface is trusted. Compromise of any VPN peer exposes all wildcard host listeners and creates a route to another LAN system.
- **Impact:** Host service compromise, credential/data access, and lateral movement to the desktop at `192.168.1.254`.
- **Remediation:** Remove `wg0` from `trustedInterfaces`; default-deny WG input; permit exact destination ports and peer `/32` source addresses; split administration, service-user, and automation peers into separate policies; restrict forwarding to the exact required destination ports and preserve source identity where possible.
- **Validation later:** Evaluate nftables/iptables rules and test allowed/denied traffic from each peer class.
- **Residual risk/trade-off:** VPN segmentation adds policy maintenance and can break peer workflows; compromised administrator peers still retain intentionally allowed management access.

## F-02 — Internet-routed Affine shares the host network namespace

- **Severity:** High
- **Status:** Confirmed statically
- **Evidence:** `modules/services/apps/affine/affine.n.nix:47-74` declares the Affine OCI container with a mutable `stable` tag, secret environment file, writable mounts, `--network=host`, and `isExposed = true`.
- **Why it matters:** A remotely exploitable Affine/container-image flaw would not be contained behind a bridge network. The process can address host loopback services and other listeners directly.
- **Impact:** Service-to-service attacks, access to databases/caches/management APIs, secret abuse, and personal-data theft.
- **Remediation:** Use a dedicated container network; publish only the required HTTP port to `127.0.0.1`; run with a fixed non-root UID/GID; minimize credentials and writable mounts; add `no-new-privileges`, capability drops, and a read-only root filesystem where supported.
- **Validation later:** Confirm the generated Docker network, container user/capabilities, and inability to connect to unrelated host-loopback ports.
- **Residual risk/trade-off:** Bridge isolation does not fix an application vulnerability by itself; Affine still requires timely updates, least-privilege credentials, and data-access controls.

## F-03 — Direct LAN services bypass the reverse-proxy boundary

- **Severity:** Medium
- **Status:** Confirmed statically
- **Evidence:**
  - `modules/services/apps/books/books.n.nix:4-11` binds Calibre to `0.0.0.0:44533`.
  - `modules/services/apps/books/books.n.nix:55-77` gives Bookbridge host networking.
  - `modules/services/apps/books/books.n.nix:95-98` opens TCP/44533 and TCP/52914 on `eth0`.
  - These services otherwise use the internal/WG service registration path.
- **Why it matters:** LAN clients can bypass Traefik TLS, routing policy, headers, and any proxy authentication added later.
- **Impact:** Ebook-library exposure, direct application attack, and a pivot through the host-networked service.
- **Remediation:** Remove LAN exceptions unless strictly required. Bind backends to loopback and route through authenticated HTTPS. If a reader protocol requires LAN access, bind only the host LAN address and allowlist only required client addresses.
- **Validation later:** Confirm listeners and test that direct LAN backend ports are unavailable except from documented clients.
- **Residual risk/trade-off:** Some ebook clients may require native LAN protocols; preserve only documented flows and rely on native authentication where proxying is impossible.

## F-04 — Docker publications probably bypass intended segmentation

- **Severity:** Medium
- **Status:** Probable; runtime validation required
- **Evidence:** Address-less mappings include `51000:8080`, `7860:7860`, `50001:80`, and `8191:8191` in active service modules, including:
  - `modules/services/apps/fusion/fusion.n.nix:3-10`
  - `modules/services/apps/books/books.n.nix:80-91`
  - `modules/services/fedeizzo-dev.n.nix:2-8`
  - `modules/services/streaming.n.nix:66-69`
- **Why it matters:** Docker normally publishes these on all host addresses and installs NAT/forwarding rules. Such paths can bypass an INPUT-firewall/reverse-proxy design.
- **Impact:** Direct access to internal applications and their credentials/data.
- **Remediation:** Prefix every backend mapping with `127.0.0.1`, or attach Traefik and applications to a dedicated proxy network without publishing host ports. Add a static check rejecting `ports` entries without an explicit host address.
- **Validation later:** Inspect `ss`, Docker port bindings, and generated nftables/iptables chains from Ethernet and WG clients.
- **Residual risk/trade-off:** Loopback publications still expose applications to local compromise and host-network workloads; dedicated proxy networks provide stronger separation.

## F-05 — Root and mixer reuse a repository-visible password verifier

- **Severity:** Medium
- **Status:** Confirmed statically; Nix-store exposure is probable
- **Evidence:** `modules/hosts/framework-desktop/system/users.n.nix:3-8` and `modules/hosts/framework-desktop/users/mixer.n.nix:54-65` contain byte-identical inline `hashedPassword` values. The verifier is intentionally not reproduced here.
- **Why it matters:** Anyone with repository/history access can perform unlimited offline guessing. A recovered password works for both the normal administrative user and root, including local PAM/doas paths even though SSH password login is disabled. Inline Nix strings may also enter world-readable store artifacts.
- **Impact:** Local or repository-assisted privilege escalation to root.
- **Remediation:** Rotate both passwords; use distinct high-entropy values; preferably lock direct root password authentication; store verifiers in separate SOPS secrets and consume them through `hashedPasswordFile`. Do not interpolate decrypted values into Nix strings. Review repository history and mirrors after rotation.
- **Validation later:** Inspect evaluated user options and resulting `/etc/shadow` handling without printing verifier values.
- **Residual risk/trade-off:** SOPS prevents repository/store disclosure but not offline guessing of a weak chosen password after root compromise; maintain an independently protected recovery method.

## F-06 — Grafana uses a committed static application secret

- **Severity:** Medium
- **Status:** Confirmed statically; exact downstream impact depends on stored Grafana data
- **Evidence:** `modules/services/observability/grafana/grafana.n.nix:10-12` assigns an inline `security.secret_key` and contains a rotation TODO. The value is intentionally not reproduced here.
- **Why it matters:** Grafana uses this key to protect sensitive application material. Repository access plus a Grafana database copy may permit decryption or forgery depending on the Grafana version and stored records.
- **Impact:** Exposure of data-source credentials and compromise of monitoring/infrastructure integrations.
- **Remediation:** Generate a strong random secret, store it in SOPS, use Grafana's file/environment secret mechanism, rotate dependent credentials where warranted, and remove the old key from the current tree. Treat repository history as retaining the old value.
- **Validation later:** Verify Grafana consumes the runtime secret and review the database/integrations affected by rotation.
- **Residual risk/trade-off:** Rotation can invalidate protected Grafana records or sessions; inventory and rotate dependent credentials with a tested rollback plan.

## F-07 — The interactive development/AI identity is root-equivalent

- **Severity:** High
- **Status:** Confirmed statically
- **Evidence:**
  - `modules/hosts/framework-desktop/users/mixer.n.nix:54-65` places `mixer` in `wheel` and `docker`.
  - `modules/system/security/security.n.nix:3-10` grants wheel users doas access to root.
  - `modules/system/nix/nix.n.nix:16-26` trusts `@wheel` for Nix operations.
  - Root and mixer share an SSH authorization identity; direct root login is permitted in `modules/hosts/framework-desktop/system/networking.n.nix:52-65`.
  - Deployment uses root SSH in `modules/hosts/framework-desktop/configuration.n.nix:91-103`.
  - Mixer imports development and AI-agent tooling in `modules/hosts/framework-desktop/users/mixer.n.nix:17-45`.
- **Why it matters:** Docker membership alone is normally root-equivalent. Running untrusted repositories, generated commands, AI tools, or plugins in this session increases exposure to command execution with a short path to full root.
- **Impact:** Full host, SOPS identity, service credentials, backups, and homelab infrastructure compromise.
- **Remediation:** Remove the human account from `docker`; prefer rootless containers or a narrow management service. Remove `@wheel` from Nix trusted users unless essential. Run untrusted development/agent workloads in a non-admin account or VM. Disable direct root SSH and deploy through a dedicated account with a narrowly scoped activation command. Use unique per-device/per-purpose SSH keys, ideally hardware-backed.
- **Validation later:** Confirm Docker socket ownership, effective doas policy, Nix trusted users, and denied direct root SSH.
- **Residual risk/trade-off:** Administrative work still needs a controlled elevation path; narrowly scoped deployment and break-glass access must be tested before removing current privileges.

## F-08 — Active runtime dependencies use mutable or unpinned sources

- **Severity:** Medium
- **Status:** Confirmed statically
- **Evidence:** Active definitions use `latest` or mutable `stable` tags, including:
  - `modules/services/fedeizzo-dev.n.nix:3-6`
  - `modules/services/streaming.n.nix:66-69`
  - `modules/services/apps/fusion/fusion.n.nix:3-9`
  - `modules/services/apps/books/books.n.nix:55-88`
  - `modules/services/apps/affine/affine.n.nix:47-56`
  - Blocky fetches mutable `latest`/`main` blocklists every four hours (`modules/services/infra/blocky/blocky.n.nix:21-33`).
  - Llama-swap declares unversioned Hugging Face model references (`modules/services/ai-workflows/llama-swap/llama-swap.n.nix:85-107`).
  - The host explicitly permits insecure .NET 6 package identifiers (`modules/hosts/framework-desktop/system/nix.n.nix:4-10`), although installation in the effective closure is unproven.
- **Why it matters:** A registry/source compromise, upstream account compromise, or unreviewed mutable reference changes runtime code, data, or model behavior without a corresponding reviewed source/lock change. Affine combines this with host networking, secrets, and writable host mounts.
- **Impact:** Non-reproducible deployment, service/data compromise, and potentially host compromise.
- **Remediation:** Pin images by immutable digest or import/build them as fixed-output Nix artifacts. Pin blocklists and model artifacts by reviewed revision/hash and update them through controlled automation. Remove insecure-package allowances unless an evaluated dependency and compensating plan justify them. Add explicit non-root users, read-only roots, capability drops, `no-new-privileges`, bounded resources, and narrowly scoped mounts.
- **Validation later:** Confirm every running image digest and downloaded model/list revision equals a reviewed declaration; evaluate whether the permitted .NET 6 packages are present in the system closure.
- **Residual risk/trade-off:** Digest pinning improves integrity and reviewability but does not establish image trust or vulnerability-free contents; updates must remain prompt and reviewed.

## F-09 — Primary-host compromise can destroy backup history

- **Severity:** High
- **Status:** Confirmed same-disk local-backup exposure; remote deletion resistance needs provider/runtime validation
- **Evidence:**
  - `modules/hosts/framework-desktop/system/backup.n.nix:7-27` defines both local and Backblaze Restic jobs running with host-held credentials.
  - The local repository is `/games/local-restic-backup`.
  - `modules/hosts/framework-desktop/system/disko.n.nix:106-117` places `/games` on the same LUKS/Btrfs disk as primary persistent data.
  - `modules/hosts/framework-desktop/system/backup.n.nix:55-57` prunes remote history with `--keep-last 30`.
  - No append-only credential, object-lock/retention policy, independently administered replica, or offline copy is declared.
- **Why it matters:** Root compromise, ransomware, operator error, or disk failure can eliminate the local copy; a client with writable remote credentials can generally delete remote snapshots too. Encryption protects confidentiality, not availability.
- **Impact:** Irreversible loss of personal data and all hosted service state.
- **Remediation:** Add an independently administered immutable/offline copy; use Backblaze object lock/retention where compatible; separate backup and prune credentials; use append-only ingestion if possible; ensure the primary host cannot delete all historical copies; test restoration regularly.
- **Validation later:** Verify B2 bucket lock/IAM/versioning, attempt a safe deletion-denial test with the backup credential, and restore into an isolated machine.
- **Residual risk/trade-off:** Immutable retention increases storage cost and can retain accidentally backed-up sensitive data; use lifecycle policy and separate credentials deliberately.

## F-10 — Traefik exposes an insecure API and wildcard metrics listener

- **Severity:** Medium
- **Status:** Confirmed configuration; exact API listener needs evaluation/runtime confirmation
- **Evidence:** `modules/services/infra/traefik/traefik.n.nix:111-135` enables `api.insecure`, the dashboard, Prometheus metrics, and a wildcard `:8082` metrics entry point.
- **Threat scenario:** Every VPN peer can reach wildcard listeners because `wg0` is trusted. A compromised peer uses the API/dashboard and metrics to enumerate routers, internal names, traffic, and software behavior for follow-on attacks.
- **Impact:** Infrastructure disclosure and a larger management-plane attack surface.
- **Remediation:** Disable `api.insecure`; route the dashboard through a dedicated authenticated/admin-only router; bind metrics to loopback or a monitoring-only address; apply source-address restrictions.
- **Validation later:** Evaluate Traefik's final arguments/listeners and verify API/metrics denial from ordinary peers.
- **Residual risk/trade-off:** Authenticated monitoring endpoints still expose sensitive topology to compromised monitoring/admin identities.

## F-11 — LUKS is not paired with a declared verified-boot chain

- **Severity:** Medium
- **Status:** Confirmed for repository configuration; firmware state unknown
- **Evidence:**
  - `modules/system/base/boot.n.nix:4-7` enables systemd-boot.
  - `modules/hosts/framework-desktop/system/boot.n.nix:3-4` allows EFI-variable writes.
  - `modules/hosts/framework-desktop/system/disko.n.nix:18-45` creates an unencrypted VFAT ESP and LUKS-encrypted remainder.
  - No active Secure Boot/UKI signing module is present.
- **Threat scenario:** A physical/firmware attacker alters the boot chain to capture the LUKS passphrase or persist before the encrypted system is trusted.
- **Impact:** Defeat of at-rest confidentiality and persistent pre-OS compromise.
- **Remediation:** Deploy controlled Secure Boot signing, signed unified kernel images, firmware protections, and a tested recovery path. Consider measured boot only with a deliberate recovery-key/PCR design.
- **Validation later:** Check firmware Secure Boot state and verify installed boot signatures after implementation; test recovery before enforcement.
- **Residual risk/trade-off:** Secure Boot complicates recovery and does not protect against compromised firmware or malicious code signed by a trusted key.

## F-12 — Backup capture is not consistently application-atomic

- **Severity:** Medium
- **Status:** Confirmed configuration; actual corruption likelihood is application-specific
- **Evidence:** `modules/hosts/framework-desktop/system/backup.n.nix:38-54` captures PostgreSQL dumps as well as live PostgreSQL/InfluxDB/application paths, without a declared quiesce hook or read-only filesystem snapshot. PostgreSQL dumps are separately enabled in `modules/services/infra/postgres/postgres.n.nix:28-33`.
- **Threat scenario:** Concurrent writes or interruption during backup produce a cryptographically valid but mutually inconsistent database/application snapshot.
- **Impact:** Failed restoration, application corruption, or loss of the expected recovery point.
- **Remediation:** Prefer verified logical dumps or supported database-native base backups; quiesce SQLite/stateful applications or back up an atomic read-only Btrfs snapshot; automate restore/integrity tests.
- **Validation later:** Restore recent and older snapshots in an isolated environment and run database/application integrity checks.
- **Residual risk/trade-off:** Quiescing can reduce availability; filesystem atomicity does not guarantee cross-application transaction consistency.

## F-13 — Host-wide resource isolation is incomplete

- **Severity:** Medium
- **Status:** Probable; explicit high-risk settings are confirmed, but effective limits require evaluation and load validation
- **Evidence:**
  - `modules/hosts/framework-desktop/system/boot.n.nix:14-19` permits up to 124 GiB of GPU-pinned memory; a comment describes 4 GiB for the OS, but this is not a proven reservation.
  - `modules/services/ai-workflows/llama-swap/llama-swap.n.nix:191-207` grants unlimited memlock and device access.
  - The local hardening inventory found explicit sandboxing for some services but no consistent workload-slice strategy or limits for the cited AI/media/container workloads; upstream defaults were not evaluated.
- **Threat scenario:** Expensive AI, browser, media, or conversion requests exhaust CPU, memory, pinned memory, processes, or I/O.
- **Impact:** Loss of SSH, authentication, database, ingress, and backup availability; forced restarts may affect data integrity.
- **Remediation:** Create workload slices for ingress, databases, AI, media, and backups; configure memory/CPU/task/I/O limits, request concurrency, restart backoff, and systemd-oomd policy; reserve enough memory for recovery access.
- **Validation later:** Evaluate effective service limits and run controlled peak workloads while monitoring recovery-plane availability.
- **Residual risk/trade-off:** Tight limits can shift failure from the host to individual applications and require workload-specific tuning.

## F-14 — Restart timer has a failure mode capable of repeated broad restarts

- **Severity:** Medium
- **Status:** Probable; timer logic is confirmed, while a recurring loop requires stale or failed sentinel behavior
- **Evidence:** `modules/hosts/framework-desktop/system/deployment.n.nix:3-37` runs every minute, derives state from one Docker service timestamp, and wildcard-restarts `docker-*.service` without a success marker, lock, health check, or backoff.
- **Threat scenario:** A broad wildcard restart fails before `docker-fedeizzodev.service` refreshes the timestamp used as the sentinel. The next minute's comparison remains eligible and retries the wildcard restart; missing or unparsable timestamps instead tend to fail the script rather than prove the comparison true.
- **Impact:** Under that failure prerequisite, repeated downtime can affect unrelated services and increase corruption risk for stateful containers.
- **Remediation:** Use per-service `restartTriggers`, deploy-time orchestration, and a persisted successfully handled generation. Restart only changed services; add locking, health checks, start limits, and alerts.
- **Validation later:** Unit-test successful and partially failed wildcard restarts, plus missing/unparsable timestamps, and observe a deployment in a disposable environment.
- **Residual risk/trade-off:** More selective orchestration is operationally complex; failed containers still need an independent recovery policy.

## F-15 — Root Backrest service has broad reach and little declared containment

- **Severity:** Medium
- **Status:** Confirmed privilege; direct listener/auth path needs evaluation/runtime validation
- **Evidence:** `modules/services/infra/backrest/backrest.n.nix:4-45` runs Backrest as root, binds `0.0.0.0:9898`, persists root configuration/data, and does not declare local capability bounding, `NoNewPrivileges`, `ProtectSystem`, `ProtectHome`, or narrow `ReadWritePaths`.
- **Threat scenario:** A Backrest vulnerability, stolen proxy identity, or direct listener path executes actions in a root process with backup/data access.
- **Impact:** Host takeover, data exfiltration, or destruction of primary and backup data.
- **Remediation:** Bind to loopback/proxy-only networking; require independent application authentication; use a dedicated user with only required paths or split privileged jobs from the UI; add strong systemd sandboxing.
- **Validation later:** Evaluate the unit, listener, native authentication, filesystem access, and effective systemd security score.
- **Residual risk/trade-off:** Backup software inherently needs broad read access; privilege separation may require separate narrowly scoped jobs and careful restore testing.

## F-16 — Grafana OAuth permits insecure email lookup

- **Severity:** Low
- **Status:** Confirmed setting; exploitability needs identity-provider validation
- **Evidence:** `modules/services/observability/grafana/grafana.n.nix:21-35` enables `oauth_allow_insecure_email_lookup` while OAuth group claims can assign Grafana administrator privileges.
- **Threat scenario:** An ambiguous, changed, or reassigned email is matched to an existing Grafana account instead of a stable issuer/subject identity.
- **Impact:** Account confusion or unintended Grafana role/admin access if identity-provider lifecycle controls fail.
- **Remediation:** Disable the setting unless required for a documented migration; bind identities by stable issuer/subject claims and validate Authentik email uniqueness and deprovisioning.
- **Validation later:** Test login, rename, deprovision, and email-reassignment cases in a non-production identity.
- **Residual risk/trade-off:** Disabling email lookup may require explicit migration of existing Grafana account bindings.

## F-17 — LAN DNS boundary and TCP behavior require validation

- **Severity:** Informational
- **Status:** Needs runtime validation
- **Evidence:** `modules/hosts/framework-desktop/system/networking.n.nix:22-28` opens UDP/53 on `eth0`; TCP/53 is not opened there. Router exposure, Blocky bind addresses, recursion ACLs, and rate limiting are not established by this excerpt.
- **Threat scenario:** Not established statically. If the resolver is forwarded externally or lacks client ACLs, third parties could abuse recursion; if TCP fallback is required but blocked, legitimate queries may fail.
- **Impact:** Potential DNS abuse or availability degradation; neither is confirmed by this repository review.
- **Remediation:** Bind Blocky explicitly to intended LAN/WG addresses, allowlist client CIDRs, configure response-rate controls, block WAN forwarding, and decide deliberately whether internal TCP/53 is needed.
- **Validation later:** Evaluate Blocky's bind/ACL settings and test UDP/TCP resolution only from authorized client networks; verify router port forwarding.
- **Residual risk/trade-off:** Strict client ACLs and rate limits can disrupt roaming VPN clients or bursty legitimate DNS traffic.

## F-18 — Active service modules open generated firewall ports

- **Severity:** Medium
- **Status:** Confirmed declarations; exact generated TCP/UDP rules require Nix evaluation
- **Evidence:** `modules/services/streaming.n.nix:9-15` enables Jellyfin with `openFirewall = true`; `modules/services/home/hass/hass.n.nix:14-20` enables Home Assistant with `openFirewall = true`. Both service modules are active through `modules/hosts/framework-desktop/configuration.n.nix`.
- **Threat scenario:** Jellyfin's module-generated firewall rules can expose its native service beyond the explicitly documented `eth0` 443/UDP 53 policy. Home Assistant also requests firewall opening, but its HTTP listener is explicitly bound to `::1`, so practical direct HTTP reachability is not established without evaluation.
- **Impact:** Confirmed firewall-policy drift and potential direct Jellyfin exposure; Home Assistant contributes an uncertain/generated rule rather than a proven reachable HTTP endpoint.
- **Remediation:** Set `openFirewall = false` where direct access is unnecessary; add explicit interface/source rules for required native protocols; document discovery requirements separately; derive an evaluated exposure manifest during CI.
- **Validation later:** Evaluate `networking.firewall`, service listener addresses, and generated nftables rules to record the exact Jellyfin/Home Assistant TCP/UDP rules and determine which have matching non-loopback listeners.
- **Residual risk/trade-off:** Home Assistant discovery and media-client features may require direct multicast or native ports; restricting them can impair device discovery and playback.

## F-19 — Mosquitto permits anonymous read/write access to all topics

- **Severity:** Medium
- **Status:** Confirmed authorization configuration; network reachability requires evaluation/runtime validation
- **Evidence:** `modules/services/home/hass/hass.n.nix:193-201` enables Mosquitto with password authentication omitted, anonymous clients allowed, and an ACL pattern granting `readwrite #` across all topics. The listener's final bind address and generated firewall behavior were not evaluated; a wildcard listener would be reachable from every peer because `wg0` is trusted.
- **Threat scenario:** A LAN, VPN, local, or compromised-service client reaches the MQTT listener and anonymously subscribes to state topics or publishes control messages to connected home-automation devices.
- **Impact:** Home activity/privacy disclosure, automation manipulation, device disruption, and potentially physical effects through controllable plugs or appliances.
- **Remediation:** Disable anonymous access; create unique credentials or client certificates for Home Assistant and Zigbee2MQTT; use least-privilege topic ACLs; bind Mosquitto to loopback or a dedicated automation network; explicitly firewall the broker from ordinary WG/LAN clients.
- **Validation later:** Evaluate the listener bind/port and firewall rules, then verify anonymous subscribe/publish is denied while only documented clients can access their required topics.
- **Residual risk/trade-off:** Credential and topic-ACL rollout can interrupt automations; preserve a tested recovery procedure and rotate credentials after confirming all clients migrate.

# Cross-cutting attack paths

## Attack path A — Compromised VPN peer to infrastructure control

```text
Compromised phone/laptop WireGuard key
→ wg0 trusted by firewall
→ enumerate wildcard services, Traefik API/metrics, SSH, and management UIs
→ exploit weak/unpatched backend or obtain reused admin identity
→ root access / SOPS identity / service credentials
→ compromise applications, backups, and downstream desktop
```

The most effective break points are removing WG interface trust, segmenting peers, binding backends to loopback, disabling direct root SSH, and independently authenticating management services.

## Attack path B — Internet application to internal services

```text
Remote vulnerability or malicious mutable image in exposed Affine
→ host network namespace
→ direct access to loopback databases/caches/management services
→ credential/data theft or service manipulation
→ lateral movement to root-capable management planes
```

The most effective break points are digest pinning, bridge/proxy-only networking, non-root containers, least-privilege credentials, and service sandboxing.

## Attack path C — Developer/AI workflow to total host compromise

```text
Malicious repository, package, prompt-generated command, plugin, or model output
→ command execution as mixer
→ Docker socket / wheel-doas / trusted Nix path
→ root
→ decrypt broad host SOPS material
→ compromise applications, VPN, DNS, backup credentials, and personal data
```

The most effective break points are isolating agent/development workloads from the admin identity, removing Docker membership, narrowing Nix/doas trust, and separating deployment credentials.

## Attack path D — Root compromise to unrecoverable loss

```text
Any root compromise
→ delete primary data and same-disk local Restic repository
→ use host-held Backblaze credential to prune/delete remote history
→ no independently immutable/offline copy remains
```

The most effective break point is a recovery copy whose deletion authority is not available to the primary host.

# Prioritized remediation roadmap

## Immediate

1. Remove `wg0` from `trustedInterfaces`; replace it with explicit peer/port policy and restrict forwarding to `192.168.1.254`.
2. Rotate the root and mixer passwords, move distinct verifiers to SOPS-backed `hashedPasswordFile`, and rotate the Grafana application secret.
3. Disable direct root SSH; create a dedicated deployment identity and unique per-device/per-purpose keys.
4. Remove `mixer` from `docker`; isolate AI/development tools from administrative credentials and privileges.
5. Pin every active OCI image by immutable digest, beginning with Internet-exposed or host-network workloads.
6. Move Affine off host networking and bind its only published backend port to loopback/proxy-only networking.
7. Establish an immutable/offline backup copy and credentials that cannot delete all remote history.
8. Disable anonymous all-topic MQTT access and issue least-privilege broker credentials before exposing the listener to any non-loopback network.

## Short term

1. Remove unintended LAN backend exceptions and bind all reverse-proxied services to loopback or dedicated proxy networks.
2. Make every Docker port mapping include an explicit host address; add a repository check that rejects ambiguous mappings and mutable tags.
3. Disable Traefik's insecure API and constrain metrics/management listeners.
4. Bind and sandbox Backrest; separate privileged backup execution from its management interface.
5. Replace the per-minute wildcard Docker restart timer.
6. Make database/application backups atomic and automate isolated restore tests.
7. Introduce systemd/container workload slices and resource limits, prioritizing AI, browser automation, media, and conversion services.
8. Disable Grafana insecure email lookup after validating Authentik identity mapping.
9. Evaluate and remove unnecessary Jellyfin/Home Assistant `openFirewall` rules; document required native/discovery flows explicitly.
10. Move `/var/container_envs/traefik` to a declared, permission-controlled, rotatable secret-provisioning mechanism.

## Longer term

1. Add Secure Boot with signed UKIs and a tested recovery process.
2. Segment the LAN into user, server, IoT, management, and backup zones with explicit inter-zone policy.
3. Separate SOPS recipients and service credentials by blast radius; plan rotation after host compromise.
4. Move backup retention/deletion authority to an independently controlled account or system.
5. Establish automated static policy checks for:
   - mutable OCI tags, model references, and downloaded blocklists
   - address-less Docker publications
   - host networking
   - inline password/application secrets
   - root services without sandboxing
   - wildcard binds for reverse-proxied services
6. Review every flake-lock update as privileged supply-chain code and periodically review third-party module changes.

# Runtime validation checklist

These checks were intentionally not executed during this static review:

- [ ] Evaluate the final NixOS configuration and record effective firewall, SSH, users, systemd, and service settings.
- [ ] Inspect `ss -lntup` and classify every listener by interface, owner, and intended trust zone.
- [ ] Inspect nftables/iptables and Docker NAT/forwarding chains, including IPv6.
- [ ] Test per-interface reachability from Ethernet and each WireGuard peer class.
- [ ] Confirm upstream router/NAT/UPnP does not expose unintended ports, especially DNS and Docker publications.
- [ ] Confirm Traefik dashboard/API and metrics are inaccessible to ordinary peers.
- [ ] Evaluate Jellyfin and Home Assistant generated firewall rules against their actual listener addresses.
- [ ] Confirm Mosquitto's bind address and verify anonymous subscribe/publish is denied after remediation.
- [ ] Confirm Backrest has native authentication and cannot be reached directly outside its intended management path.
- [ ] Inspect effective container users, capabilities, mounts, network modes, image digests, and resource limits.
- [ ] Check runtime modes/ownership for `/run/secrets`, the Age key, `/persist`, container environment files, SSH material, and backup credentials without printing their contents.
- [ ] Verify Secure Boot state and installed boot artifact signatures after implementing verified boot.
- [ ] Verify Backblaze object-lock/IAM/versioning behavior and that ordinary backup credentials cannot delete retained history.
- [ ] Restore recent and older snapshots to an isolated machine and run database/application integrity checks.
- [ ] Validate Authentik subject/email uniqueness, group-to-admin mapping, account deprovisioning, and MFA policy.
- [ ] Scan Git history and remote mirrors for retired plaintext secrets, password verifiers, and application keys after rotation.
- [ ] Exercise controlled AI/media/backup load to tune limits while preserving SSH, ingress, database, and authentication availability.

# Residual risk and confidence

The conclusions are strongest for explicit local declarations: WG trust, unrestricted forwarding, host networking, direct LAN openings, reused inline password verifiers, root-equivalent groups, direct root SSH, mutable sources, anonymous MQTT authorization, backup topology, and the broad restart timer. Repeated restart behavior remains a conditional failure mode rather than a confirmed recurring outage.

Final exposure can differ because NixOS and imported module defaults, Docker firewall integration, IPv6, external routers, provider-side controls, and live application authentication were not evaluated. Those uncertainties are explicitly labeled rather than assumed safe or vulnerable.

The repository's declarative structure makes the main risks remediable and testable. The largest reduction in practical risk will come from narrowing trust zones—not merely adding more authentication—then separating routine development from root authority and making recovery data independently undeletable.

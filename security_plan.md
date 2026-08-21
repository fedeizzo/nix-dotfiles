# NixOS Homelab Security Review Plan

## 1. Objective

Perform an evidence-based **static security review** of the NixOS configuration composed for `modules/hosts/framework-desktop/`, starting from `modules/dendritic.nix` and following every transitive module import that affects the host.

The review will identify configuration weaknesses, insecure defaults, trust-boundary violations, secret exposure, availability risks, and plausible attack paths. It will not modify the system or probe a live host.

## 2. Agreed Scope

### In scope

- `modules/dendritic.nix`
- `modules/hosts/framework-desktop/`
- Every local module transitively imported into the framework-desktop NixOS configuration
- Relevant flake inputs, overlays, packages, and module arguments used by that composition
- Home Manager configuration where it affects the host's security posture
- Declarative network services, containers, virtual machines, filesystems, backups, authentication, and secret-management configuration
- Repository-tracked files that may expose credentials or weaken the composed host

### Out of scope for this review

- Live host inspection
- Port scanning or service probing
- Exploit attempts
- Runtime log analysis
- Changing files other than review documentation, unless separately approved
- Other hosts except where shared modules or trust relationships affect framework-desktop

Findings that require runtime confirmation will be labeled **Needs runtime validation** and supplied with a safe validation procedure rather than treated as confirmed.

## 3. Threat Model

The review assumes a combination of:

1. **Internet attackers** targeting exposed services, authentication, known-vulnerable software, and remote-code-execution paths.
2. **LAN adversaries** operating from a compromised or untrusted device inside the home network.
3. **Local attackers** attempting privilege escalation, credential theft, persistence, or bypass of system controls.
4. **Supply-chain attackers** influencing flake inputs, packages, update sources, build inputs, or third-party modules.

### Crown jewels

- Credentials: passwords, SSH keys, API tokens, certificates, recovery material, and identity-provider access
- Personal data: documents, databases, photos, backups, and private service data
- Availability: reliable service, recoverability, and resistance to destructive actions or resource exhaustion
- Infrastructure control: root access, network control, service administration, containers/VMs, and lateral-movement paths

## 4. Review Method

### Phase 1 — Resolve the effective configuration graph

- Identify the framework-desktop NixOS configuration entry point.
- Trace imports beginning with `modules/dendritic.nix` and the framework-desktop host modules.
- Record shared NixOS and Home Manager modules that materially affect security.
- Identify conditional configuration, `specialArgs`, overlays, and input-derived modules.
- Distinguish settings that are definitely active from definitions that are not imported by this host.

**Output:** a concise import/configuration map and a list of security-relevant components actually composed into the host.

### Phase 2 — Audit trust and supply-chain controls

Review:

- Flake input pinning and lock-file usage
- Nonstandard or mutable package sources
- Overlays, `fetch*` calls, unsigned downloads, and unverified binaries
- Third-party NixOS/Home Manager modules
- Impure evaluation or environment-dependent configuration
- Automatic updates, rollback behavior, and update provenance
- Packages or services with elevated privileges or broad host access

### Phase 3 — Audit secrets and identity

Review:

- Plaintext secrets or sensitive values committed to the repository
- Secret-management mechanisms and decryption boundaries
- File ownership, permissions, and secret placement in the Nix store
- User accounts, groups, password policy, and administrative access
- `sudo`, `doas`, polkit, PAM, and passwordless privilege escalation
- SSH authentication, root login, agent forwarding, and trusted keys
- Service accounts and unnecessary interactive shells
- Credential sharing across services or trust zones

No secret values will be reproduced in the report. Evidence will reference locations and redact sensitive material.

### Phase 4 — Audit network exposure and segmentation

Review:

- Firewall enablement, defaults, allowed ports, interfaces, and trusted zones
- TCP/UDP listeners declared by services
- Services bound to wildcard or public interfaces
- Reverse proxies, TLS, certificate handling, and upstream trust
- VPN configuration and routes
- DNS, mDNS, discovery protocols, and LAN exposure
- Container/VM network boundaries
- Authentication on administrative interfaces
- Proxy-header trust and direct-backend bypass paths
- Lateral-movement opportunities between host services and other homelab systems

A static exposure matrix will map each service to its intended interface, port, authentication, encryption, and data sensitivity where the configuration provides enough evidence.

### Phase 5 — Audit host hardening and isolation

Review:

- Secure Boot, full-disk encryption declarations, bootloader protections, and kernel parameters
- Kernel hardening and sysctl settings
- NixOS security options and potentially dangerous compatibility settings
- Service sandboxing (`systemd` hardening directives and NixOS module defaults)
- Linux capabilities, setuid programs, device access, and privileged ports
- Containers, virtualization, namespaces, and host filesystem mounts
- Desktop attack surface, portals, remote access, and local discovery services
- Persistence mechanisms, scheduled jobs, and writable executable paths
- Logging/auditing declarations and protection of security-relevant logs

### Phase 6 — Audit data protection, backups, and availability

Review:

- Filesystem and dataset permissions
- Encryption at rest and in transit where declaratively visible
- Backup targets, credentials, retention, immutability, and restore assumptions
- Whether a compromised host can delete both primary data and backups
- Service restart policy, dependency ordering, and single points of failure
- Resource limits and obvious denial-of-service risks
- Update/rollback strategy and recovery access
- Exposure of management planes that could disable the homelab

### Phase 7 — Build attack paths and prioritize remediation

Correlate individual weaknesses into plausible scenarios, such as:

- Internet-facing service compromise → service credential theft → lateral movement
- LAN compromise → unauthenticated management endpoint → infrastructure takeover
- Repository or flake-input compromise → privileged code execution during rebuild
- Local desktop compromise → secret extraction → backup or server compromise
- Host compromise → deletion/encryption of both data and reachable backups

Prioritize fixes that break multiple attack paths or protect multiple crown jewels.

## 5. Finding Standard

Each finding will contain:

- **Title and severity**
- **Status:** confirmed statically, probable, or needs runtime validation
- **Affected file(s) and option(s)**
- **Evidence** from the effective import path and configuration
- **Threat scenario** and prerequisites
- **Impact** on credentials, personal data, availability, or infrastructure
- **Recommended remediation**, preferably as a concrete Nix change
- **Validation steps** that can later confirm the fix safely
- **Residual risk** or compatibility trade-offs

### Severity scale

- **Critical:** likely compromise of root/infrastructure or major sensitive data from a realistically reachable path; urgent remediation
- **High:** substantial confidentiality, integrity, or availability impact with plausible exploitation
- **Medium:** meaningful weakness requiring additional access, chaining, or uncommon conditions
- **Low:** defense-in-depth gap or limited-impact issue
- **Informational:** relevant observation, inventory item, or hardening opportunity without a demonstrated vulnerability

Severity will reflect both impact and exploitability. Missing evidence will not be presented as a confirmed vulnerability.

## 6. Deliverables

1. **Configuration/import map** for framework-desktop
2. **Attack-surface inventory** of users, privileges, services, ports, trust boundaries, secrets mechanisms, and important data paths
3. **Prioritized findings report** with file-level evidence
4. **Attack-path summary** showing how findings may combine
5. **Remediation roadmap** divided into:
   - Immediate risk reduction
   - Short-term hardening
   - Longer-term architectural improvements
6. **Runtime validation checklist** for uncertainties that static configuration cannot resolve
7. Optional patch set only after explicit approval

## 7. Safety and Handling Rules

- Keep the review read-only except for review documentation.
- Do not decrypt, print, or copy secrets.
- Do not run the host configuration, contact configured services, or probe network targets.
- Do not infer that a module definition is active without tracing it into framework-desktop.
- Separate confirmed evidence from assumptions and recommendations.
- Avoid changing availability-sensitive settings without explaining operational impact and rollback requirements.
- Obtain explicit approval before moving from this plan to implementation or any runtime validation.

## 8. Completion Criteria

The static review is complete when:

- All transitive security-relevant imports for framework-desktop have been traced.
- Every identified network service and privileged component has been reviewed.
- Secrets, identity, supply chain, host hardening, isolation, data protection, backup, and availability controls have been assessed.
- Findings include evidence, severity, remediation, and confidence/status.
- Static limitations and required runtime checks are documented.

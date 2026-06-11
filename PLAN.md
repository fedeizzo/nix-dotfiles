# Dendritic Pattern Migration Plan

## Objective
Migrate the current NixOS, Nix-Darwin, and Home-Manager configurations to the **Dendritic Pattern** using the **Flake Parts Framework** (`flake.modules`). This will flatten the repository structure, organizing code by **feature** rather than target system.

---

## Phase 1: Architectural Foundation

1. **Modules Directory:** Create a `modules/` directory at the root of the repository.
2. **Dynamic Inputs (`flake-file`):** Adopt `flake-file` to replace the static `flake.nix`. This allows individual feature modules to declare their own flake dependencies (inputs). `flake-file` will be used as a generator to bake out the final static `flake.nix`.
3. **Auto-Discovery:** Use `import-tree` (or `flake-file`'s built-in tools) to automatically and recursively import all `.nix` files inside the `modules/` directory into the `flake-parts` evaluation.
4. **Configuration Exports:** Set up the evaluation of `nixosConfigurations` and `darwinConfigurations` dynamically from the top-level `flake.modules.nixos.<host>` and `flake.modules.darwin.<host>` outputs.

---

## Phase 2: Macro Categories

Before listing the categories, it is important to clarify the terminology used in the Dendritic pattern:
- **Feature (The "What"):** The logical goal or chunk of functionality from a conceptual perspective (e.g., "Git", "Gnome", "Basic CLI Tools").
- **Aspect (The "How"):** The structural code pattern used to implement that feature. 
  - *Example:* When configuring the **feature** "Git", you must decide how to implement it structurally. If Git only requires user-level configuration, it is written as a **Simple Aspect**. If Git requires both system packages (NixOS) and user settings (Home-Manager), it is written as a **Multi-Context Aspect**.

Code will be organized into feature-based files. The folder structure is for human readability, as the framework merges everything into `flake.modules`.

### Module Classes & Motivations
We define specific classes under `flake.modules` to strictly separate concerns. Here are the required classes and their motivations:

1. **`n` = `nixos`** (`flake.modules.nixos`)
   - **Motivation:** Handles system-level configurations (services, global packages, hardware, bootloaders). This replaces the traditional `lib.nixosSystem` instantiations.
2. **`h` = `homeManager`** (`flake.modules.homeManager`)
   - **Motivation:** Manages user-level dotfiles and CLI settings. Separating this from `nixos` is critical for decoupling: it ensures dotfiles can be used on non-NixOS systems (like Arch or macOS) and prevents hardcoding usernames into system files.
3. **`d` = `darwin`** (`flake.modules.darwin`)
   - **Motivation:** For `nix-darwin` (macOS) system configurations. Even if not heavily used immediately, defining it ensures cross-platform readiness.
4. **`k` = `disko`** (`flake.modules.disko`)
   - **Motivation:** Declarative disk partitioning is complex and highly machine-specific. Isolating it into its own class keeps host definitions clean and prevents disk schemas from leaking into general hardware features.
5. **`g` = `generic`** (`flake.modules.generic`)
   - **Motivation:** Used exclusively for **Constants Aspects** and **DRY Aspects**. This class holds raw data, shared variables (like an admin email), or reusable attribute subsets (like subnet routing tables) that don't belong to a specific OS or user context.

### Naming Convention & Validation
We will use lowercase dot notation to visually indicate which of the above module classes are configured within a given file: `<feature>.<classes>.nix`.

*Example:* `git.nh.nix` dictates that the file configures both `flake.modules.nixos` and `flake.modules.homeManager`.

To enforce this architecture, we will implement a **validation script**. This script will be natively integrated into your repository via `nix/git-hooks.nix` (using `pre-commit-hooks.nix`). The hook will parse each modified `.nix` file in the `modules/` directory and ensure that the classes defined inside perfectly sync with the letters in the filename, automatically blocking any commits that violate the Dendritic architecture!

* **`modules/core/`**: Fundamental settings (Nix daemon, sops, deploy-rs, base user configs).
* **`modules/hardware/`**: Bare metal settings (Disks, sound, bluetooth, networking, hardware quirks).
* **`modules/desktop/`**: Display environments (Niri, Wayland, Stylix).
* **`modules/apps/`**: GUI applications (Firefox, Emacs, Kitty, Zed).
* **`modules/cli/`**: Terminal utilities (Fish, Starship, Git).
* **`modules/dev/`**: Development environments (Languages, Devshells, LLMs).
* **`modules/hosts/`**: The final consumers (e.g., `oven`, `homelab`, `macbook-pro`).

---

## Phase 3: Applying Aspect Patterns (Iterative Migration)

When migrating features from `home/` and `hosts/` into `modules/`, the following design patterns will be applied:

### 1. Multi Context Aspects
Used when a feature spans across the system level and the user level (e.g., system packages + Home-Manager settings).
- **Structure:** Defines a main module (NixOS/Darwin) and an auxiliary module (Home-Manager). The main module automatically injects the auxiliary module into `home-manager.sharedModules`.
- **Importing on NixOS:** Simply import `inputs.self.modules.nixos.<feature>`. The system configures itself and automatically injects the Home-Manager settings into your user environment.
- **Importing on non-NixOS (e.g., Arch Linux):** Import `inputs.self.modules.homeManager.<feature>` directly into your `homeConfigurations`. Due to Nix's lazy evaluation, the NixOS system code is safely ignored, ensuring complete portability.

### 2. Simple Aspects
Used for features that are entirely isolated to one context (e.g., a pure NixOS service or a Darwin-specific setting).
- **Structure:** Defined independently for the specific class without relying on other modules.

### 3. Collector Aspects
Used when a feature's configuration needs to aggregate data provided by other features (like collecting peer keys for a VPN or Syncthing).
- **Structure:** The base feature (e.g., `modules/network/wireguard.nix`) enables the service. The contributor features (e.g., individual host files like `modules/hosts/oven.nix`) do not put their keys in their own host block. Instead, they define a `flake.modules.nixos.wireguard = { ... };` block. Because all files are imported, the Nix module system automatically merges these blocks into a single comprehensive WireGuard module that contains every peer, which any host can then inherit!

### 4. Inheritance Aspects (Profiles & Hosts)
Used to aggregate features for specific machines or logical profiles.
- **Structure:** The final host configurations (e.g., `modules/hosts/oven.nix`) will use `imports = [...]` to inherit the required aspects from `inputs.self.modules.<class>`.

### Feature Mapping & Aspect Categorization
Based on a deep analysis of your current repository (`home/common`, `home/macbook-pro`, `home/x1-nano`, and `hosts/framework-desktop`), here is a highly refined mapping of your features into Dendritic Aspect patterns:

#### 1. Simple Aspects (Isolated Contexts)
- **`sops.n.nix`**: Secret management (NixOS).
- **`disko.k.nix`**: Declarative disk layouts (`framework-desktop`, `x1-nano`).
- **`backup.n.nix`**: Server backup routines (found in `framework-desktop/system/backup.nix`).
- **`zsh.h.nix`**: macOS default shell configuration (found in `home/macbook-pro`).
- **`plasma.nh.nix`**: KDE Plasma enablement (found in `home/x1-nano/modules/plasma`).

#### 2. Multi-Context & Conditional Aspects (The Big Refactors!)
Currently, you have `firefox` and `zed` defined in *both* `home/common/` and `home/macbook-pro/`. Dendritic completely eliminates this duplication.
- **`firefox.nhd.nix`**: A single file that installs Firefox on NixOS (`n`), configures universal Home-Manager dotfiles (`h`), and uses a **Conditional Aspect** (`lib.mkIf pkgs.stdenv.isDarwin`) to inject the macOS-specific tweaks (`d`).
- **`zed.nhd.nix`**: Same logic as Firefox.
- **`fish.nh.nix`**: Installs fish globally on NixOS and configures universal plugins/aliases.

#### 3. Inheritance Aspects (Profiles)
- **`x1-nano.n.nix`**, **`framework-desktop.n.nix`**, **`macbook-pro.d.nix`**: The entry points. They just `import` features.
- **`profile-desktop.n.nix`**: An intermediate inheritance aspect that groups GUI apps together, so `x1-nano` doesn't have to list 50 apps manually.

#### 4. Collector Aspects (Distributed Aggregation)
- **`topology.n.nix`**: (Found in `framework-desktop/system/topology.nix`). Instead of defining the whole network map centrally, every machine (like `x1-nano`) will contribute its own `flake.modules.nixos.topology` definition. The `framework-desktop` will collect them all and draw the graph automatically!
- **`wireguard.n.nix`**: Each host contributes its public key to the central mesh.

#### 5. Constants & DRY Aspects (Reusability)
- **`impermanence.g.nix`**: You use `persistent.nix` heavily across hosts. Instead of duplicating the impermanence paths, you can define shared lists (like `/var/log`, `/var/lib/nixos`) as a **DRY Aspect** and merge them into each host's impermanence configuration.
- **`systemConstants.g.nix`**: Shared variables like your admin email, timezone, or primary SSH port.

---

## Safe, Incremental Migration Plan
To avoid breaking the entire system at once, the migration will be done **incrementally**. You can migrate one app or one host at a time while the rest of the system remains untouched.

### Execution Strategy
We are utilizing an iterative, LLM-assisted workflow:
- **New Revision**: Create a new revision for the feature.
- **Simple Module**: The LLM focuses strictly on writing the code for a single, isolated module.
- **Review**: The user manually reviews the LLM's generated module.
- **Test**: The user tests the configuration to ensure it functions correctly.
- **Repeat**: Once validated, move on to the next module.

*(Note: The LLM's sole responsibility is generating the module code; the rest of the workflow is managed by the user).*

**Step 1: The Scaffolding (Zero breaking changes)**
- Integrate `flake-parts`, `flake-file`, and `import-tree` into your repository.
- Create the `modules/` directory and set up the validation script.
- *Status:* Your existing `nixosConfigurations` in `nix/nixosConfigurations.nix` continue to build exactly as before.

**Step 2: The First Feature (Proof of Concept)**
- Create a single feature in the new architecture (e.g., `modules/cli/git.h.nix`).
- Import this new feature into your *existing* legacy host configurations. 
- *Status:* You now have a hybrid system where one feature is Dendritic, and the rest is legacy.

**Step 3: Gradual Refactoring (At your own pace)**
- Move your apps from `home/common/` one by one into `modules/apps/`.
- Move your system settings from `hosts/` into `modules/core/`.
- Test your configurations continuously after each small batch.

**Step 4: The Final Switch**
- Once all features and hosts have been moved into `modules/`, delete the old `home/` and `hosts/` directories.
- Flip the switch to fully rely on the automated `flake-parts` outputs and remove `nix/nixosConfigurations.nix`.

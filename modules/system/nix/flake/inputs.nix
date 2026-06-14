{

  # ── Core nixpkgs ──────────────────────────────────────────────────────
  flake-file.inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
  flake-file.inputs.nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

  # ── Flake management ──────────────────────────────────────────────────
  flake-file.inputs.nixos-generators.url = "github:nix-community/nixos-generators";

  # ── Remote deployment ─────────────────────────────────────────────────
  flake-file.inputs.deploy-rs.url = "github:serokell/deploy-rs";

  # ── Misc / no better home ─────────────────────────────────────────────
  flake-file.inputs.nixos-pikvm.url = "github:hatch01/nixos-pikvm"; # no module yet
}

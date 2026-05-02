{ pkgs-unstable, system, ... }:
{
  imports = [
    (pkgs-unstable + /nixos/modules/services/networking/llama-swap.nix)
    # (pkgs-unstable + /nixos/modules/services/misc/jellyseerr.nix)
    # (pkgs-unstable + /nixos/modules/services/home-automation/home-assistant.nix)
    (pkgs-unstable + /nixos/modules/services/misc/paperless.nix)
  ];
  nixpkgs.overlays = [
    (_: _: {
      inherit (pkgs-unstable.legacyPackages.${system}) llama-swap;
      inherit (pkgs-unstable.legacyPackages.${system}) llama-rocm;
      # inherit (pkgs-unstable.legacyPackages.${system}) jellyseerr;
      # inherit (pkgs-unstable.legacyPackages.${system}) home-assistant;
      inherit (pkgs-unstable.legacyPackages.${system}) paperless;
    })
  ];
  disabledModules = [
    "services/networking/llama-swap.nix"
    # "services/misc/jellyseerr.nix"
    # "services/home-automation/home-assistant.nix"
    "services/misc/paperless.nix"
  ];
}

{ pkgs-unstable, system, ... }:
{
  imports = [
    (pkgs-unstable + /nixos/modules/services/misc/ollama.nix)
    (pkgs-unstable + /nixos/modules/services/misc/jellyseerr.nix)
    (pkgs-unstable + /nixos/modules/services/home-automation/home-assistant.nix)
    (pkgs-unstable + /nixos/modules/services/misc/paperless.nix)
  ];
  nixpkgs.overlays = [
    (_: _: {
      inherit (pkgs-unstable.legacyPackages.${system}) ollama-rocm;
      inherit (pkgs-unstable.legacyPackages.${system}) jellyseerr;
      inherit (pkgs-unstable.legacyPackages.${system}) home-assistant;
      inherit (pkgs-unstable.legacyPackages.${system}) paperless;
    })
  ];
  disabledModules = [
    "services/misc/ollama.nix"
    "services/misc/jellyseerr.nix"
    "services/home-automation/home-assistant.nix"
    "services/misc/paperless.nix"
  ];
}

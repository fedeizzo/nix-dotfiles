{
  flake.modules.darwin.nix-core = { system-overlays, ... }: {
    system.stateVersion = 4;
    nix = {
      enable = true;
      settings = {
        experimental-features = "nix-command flakes";
      };
      extraOptions = ''
        auto-optimise-store = false
        experimental-features = nix-command flakes
        extra-platforms = x86_64-darwin aarch64-darwin
      '';
      optimise.automatic = true;
    };
    nixpkgs.overlays = builtins.attrValues system-overlays;
    ids.gids.nixbld = 350;
  };
}

{
  flake.modules.darwin.nix-core = { ... }: {
    system.stateVersion = 4;
    nix = {
      enable = true;
      settings = {
        experimental-features = "nix-command flakes";
        extra-substituters = [
          "https://vicinae.cachix.org"
        ];
        extra-trusted-public-keys = [
          "vicinae.cachix.org-1:1kDrfienkGHPYbkpNj1mWTr7Fm1+zcenzgTizIcI3oc="
        ];
      };
      extraOptions = ''
        auto-optimise-store = false
        experimental-features = nix-command flakes
        extra-platforms = x86_64-darwin aarch64-darwin
      '';
      optimise.automatic = true;
    };
    ids.gids.nixbld = 350;
  };
}

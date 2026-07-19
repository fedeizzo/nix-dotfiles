{
  flake.modules.darwin.nix-core = { ... }: {
    system.stateVersion = 4;
    nix = {
      enable = true;
      settings = {
        experimental-features = "nix-command flakes";
        extra-substituters = [
          "https://vicinae.cachix.org"
          "https://nix-amd-ai.cachix.org"
        ];
        extra-trusted-public-keys = [
          "vicinae.cachix.org-1:1kDrfienkGHPYbkpNj1mWTr7Fm1+zcenzgTizIcI3oc="
          "nix-amd-ai.cachix.org-1:F4OU4vw/lV2oiG6SBHZ+nqjl4EFJuqI4X9A7pvaBmhQ="
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

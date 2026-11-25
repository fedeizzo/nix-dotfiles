{
  flake.modules.homeManager.cli-nix = { pkgs, lib, ... }: {
    home.packages = lib.optionals pkgs.stdenv.isLinux (with pkgs; [
      cachix
    ]);
  };
}

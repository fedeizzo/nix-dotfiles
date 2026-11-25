{
  flake.modules.homeManager.cli-vcs = { pkgs, lib, ... }: {
    home.packages = with pkgs; [
      git-crypt
    ] ++ lib.optionals pkgs.stdenv.isLinux [
      gh
    ];
  };
}

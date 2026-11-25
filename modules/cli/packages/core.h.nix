{
  flake.modules.homeManager.cli-core = { pkgs, lib, ... }: {
    home.packages = with pkgs; [
      eza
      fd
      fzf
      gawk
      gnused
      bat
      ripgrep
      home-manager
    ] ++ lib.optionals pkgs.stdenv.isLinux [
      zip
      unzip
      jq
      coreutils
      moreutils
      file
      rsync
    ];
  };
}

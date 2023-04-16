{ pkgs, ... }:

{
  config = {
    home.packages = with pkgs; [
      exa
      fd
      fzf
      gawk
      gnused
      bat
      ripgrep
      pandoc
      graphviz
      ffmpeg
      home-manager
    ];
  };
}

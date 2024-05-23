{ pkgs, pkgs-unstable, ... }:

{
  config = {
    home.packages = with pkgs; [
      eza
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
      git-crypt
      rbw
      pkgs-unstable.pet
    ];

    # programs.pet = {
    #   enable = true;
    #   settings = {
    #     General = {
    #       cmd = [ "zsh" "-c" ];
    #       sortBy = "recency";
    #     };
    #   };
    # }
  };
}

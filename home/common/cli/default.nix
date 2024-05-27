{ pkgs, ... }:

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
      pet
      hurl
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

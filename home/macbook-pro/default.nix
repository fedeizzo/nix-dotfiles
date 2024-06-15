{ inputs, pkgs, username, ... }:


{
  home = {
    username = username;
    stateVersion = "24.05";
    homeDirectory = "/Users/${username}";
  };

  imports = [
    ../common/emacs
    ./modules/languages
    ./modules/cli
    ./modules/zsh
    ../common/starship
  ];

  home.packages = with pkgs; [ pngpaste ];
}

{ pkgs, username, ... }:


{
  home = {
    inherit username;
    stateVersion = "25.05";
    homeDirectory = "/Users/${username}";
  };

  imports = [
    # ../common/emacs
    ../common/languages
    ./modules/cli
    ./modules/zsh
    ../common/starship
    ./modules/firefox
    ./modules/zed
  ];

  home.packages = with pkgs; [ pngpaste ];
}

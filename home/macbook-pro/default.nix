{ pkgs, username, ... }:


{
  home = {
    inherit username;
    stateVersion = "24.11";
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

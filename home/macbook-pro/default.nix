{ pkgs, username, ... }:


{
  home = {
    inherit username;
    stateVersion = "24.05";
    homeDirectory = "/Users/${username}";
  };

  imports = [
    ../common/emacs
    ./modules/languages
    ./modules/cli
    ./modules/zsh
    ../common/starship
    ../common/wezterm
    ./modules/firefox
  ];

  home.packages = with pkgs; [ pngpaste ];
}

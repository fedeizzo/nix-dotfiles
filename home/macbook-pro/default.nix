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
    inputs.self.modules.homeManager.zsh
    inputs.self.modules.homeManager.starship
    inputs.self.modules.homeManager.git
    inputs.self.modules.homeManager.zen
    inputs.self.modules.homeManager.zed
  ];

  home.packages = with pkgs; [ pngpaste ];
}

{ pkgs, username, inputs, ... }:


{
  home = {
    inherit username;
    stateVersion = "25.05";
    homeDirectory = "/Users/${username}";
  };

  imports = [
    # ../common/emacs
    ../common/languages
    inputs.self.modules.homeManager.direnv
    inputs.self.modules.homeManager.cli-packages
    inputs.self.modules.homeManager.jujutsu
    inputs.self.modules.homeManager.zsh
    inputs.self.modules.homeManager.starship
    inputs.self.modules.homeManager.git
    inputs.self.modules.homeManager.zen
    inputs.self.modules.homeManager.zed
    inputs.self.modules.homeManager.profile-work
  ];

  home.packages = with pkgs; [ pngpaste ];
}

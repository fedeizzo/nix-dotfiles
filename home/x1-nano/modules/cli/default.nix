{ pkgs, ... }:

{
  imports = [
    ../../../common/cli/linux
    inputs.self.modules.homeManager.jujutsu
  ];

  home.packages = [
    pkgs.gcc
  ];

  programs.jail-pi = {
    enable = true;
    persistName = "pi";
    allowNetwork = true;
  };
}

{ pkgs, ... }:

{
  imports = [
    ../../../common/cli/linux
    ../../../common/jujutsu
  ];

  home.packages = [
    pkgs.gcc
  ];

  programs.jujutsu.settings.user.email = "federico@fedeizzo.dev";

  programs.jail-pi = {
    enable = true;
    persistName = "pi";
    allowNetwork = true;
  };
}

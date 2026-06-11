{ pkgs, ... }:

{
  imports = [
    ../../../common/cli/default.nix
    inputs.self.modules.homeManager.jujutsu
  ];
  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
  };
}

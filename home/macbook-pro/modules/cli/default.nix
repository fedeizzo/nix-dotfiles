{ pkgs, inputs, ... }:

{
  imports = [ ../../../common/cli/default.nix ];
  home.packages = [

    inputs.deploy-rs.defaultPackage.aarch64-darwin
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
}

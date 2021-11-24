{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    # cudatoolkit_11
    # cudnn_cudatoolkit_11
  ];
}

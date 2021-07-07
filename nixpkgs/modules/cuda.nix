{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    # cudatoolkit_11_2
    # cudnn_cudatoolkit_11_2
  ];
}

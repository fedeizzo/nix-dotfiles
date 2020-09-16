{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    cudatoolkit_10_1
    cudnn_cudatoolkit_10_1
  ];
}

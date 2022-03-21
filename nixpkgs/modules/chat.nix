{  config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    unstable.tdesktop
  ];
}

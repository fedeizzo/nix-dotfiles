{ pkgs, ... }:

{
  imports = [ ../../../common/cli/linux ];
  home.packages = with pkgs; [
    htop
    nmap
    powertop
    xsv
    zsh
    brightnessctl
    pamixer
    borgbackup
    ncdu
    playerctl
    libsecret
    sshfs
  ];
}

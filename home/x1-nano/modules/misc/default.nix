{ pkgs, pkgs-unstable, ... }:

{
  programs.jail-pi = {
    enable = true;
    persistName = "pi";
    allowNetwork = true;
  };
  home.packages = with pkgs; [
    gcc
    moonlight-qt
    pavucontrol
    vlc
    bitwarden-desktop
    telegram-desktop
    ddcui # control external monitor
    ddcutil

    # 3d printing
    pkgs-unstable.orca-slicer
    freecad
    openscad

    # notes
    pkgs.anytype

    element-desktop
    loupe

    # pkgs-unstable.pi-coding-agent

    art

    pkgs.llm-agents.antigravity-cli
  ];
}

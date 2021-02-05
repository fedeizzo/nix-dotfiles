{ config, pkgs, libs, ... }:

{
  home.file.".xprofile" = {
    source = ../dotfiles/dot_xmonad/xmonad-session-rc;
    executable = true;
  };
  home.file.".xmonad/xmonad.hs".source = ../dotfiles/dot_xmonad/xmonad.hs;
  home.file.".xmonad/xmobar.hs".source = ../dotfiles/dot_xmonad/xmobar.hs;
  home.file.".xmonad/bin/trayer-padding-icon" = {
    source = ../dotfiles/dot_xmonad/bin/trayer-padding-icon;
    executable = true;
  };
}

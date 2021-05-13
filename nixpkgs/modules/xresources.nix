{ config, pkgs, libs, ... }:

{
  xresources.properties = {
    "Xft.dpi" = 96;
    "Xcursor.size" = 16;
  };
}

{ config, pkgs, ... }:

{
  security.sudo = {
    enable = true;
    whellNeedsPassword = true;
  };
}

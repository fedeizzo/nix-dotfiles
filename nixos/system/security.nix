{ config, pkgs, ... }:

{
  #################################
  # SECURITY
  #################################
  # TODO move to doas
  security.sudo = {
    enable = true;
    wheelNeedsPassword = true;
  };
}

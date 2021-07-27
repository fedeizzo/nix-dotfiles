{ config, pkgs, ... }:

{
  #################################
  # SECURITY
  #################################
  security.sudo = {
    enable = true;
    wheelNeedsPassword = true;
  };
  # dell
  # security.doas = {
  #   enable = true;
  #   extraRules = [
  #     { groups = [ "wheel" ]; keepEnv = true; persist = true; }
  #   ];
  # };
}

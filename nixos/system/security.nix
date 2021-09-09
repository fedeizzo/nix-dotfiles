{ config, pkgs, ... }:

{
  #################################
  # SECURITY
  #################################
  security.pam.services.lightdm.enableGnomeKeyring = true;
  security.pam.services.lightdm-greeters.enableGnomeKeyring = true;
  security.pam.services.login.fprintAuth = true;
  security.pam.services.system-local-login.fprintAuth = true;
  security.pam.services.lightdm.fprintAuth = true;
  security.pam.services.doas.fprintAuth = true;
  security.sudo = {
    enable = false;
  };
  security.doas = {
    enable = true;
    extraRules = [
      { groups = [ "wheel" ]; keepEnv = true; persist = true; }
    ];
  };
}

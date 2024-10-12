{ pkgs, ... }:

{
  security = {
    pam.services = {
      login.fprintAuth = true;
      system-local-login.fprintAuth = true;
      hyprlock = {
        unixAuth = true;
        fprintAuth = true;
      };
    };
    sudo.enable = false;
    doas = {
      enable = true;
      fprintAuth = true;
      extraRules = [
        { groups = [ "wheel" ]; keepEnv = true; persist = true; }
      ];
    };
  };

  programs = {
    gnupg.agent.enable = true;
    gnupg.agent.pinentryPackage = pkgs.pinentry-qt;
    ssh.askPassword = "";
  };

  environment.systemPackages = [ pkgs.pinentry-qt ];
}

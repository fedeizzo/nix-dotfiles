{ pkgs, ... }:

{
  security.pam.services.login.fprintAuth = true;
  security.pam.services.system-local-login.fprintAuth = true;
  security.pam.services.doas.fprintAuth = true;
  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [
      { groups = [ "wheel" ]; keepEnv = true; persist = true; }
    ];
  };
  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryFlavor = "qt";
  programs.ssh.askPassword = "";

  environment.systemPackages = with pkgs; [ pinentry-qt ];
}

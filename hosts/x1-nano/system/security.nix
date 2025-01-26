{ pkgs, ... }:

{
  security = {
    sudo.enable = false;
    doas = {
      enable = true;
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

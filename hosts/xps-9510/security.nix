{ config, pkgs, username, ... }:

{
  security.pam.services.lightdm.enableGnomeKeyring = false;
  security.pam.services.lightdm-greeters.enableGnomeKeyring = false;
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

  # secrets management
  environment.systemPackages = with pkgs; [ sops age ];
  sops = {
    defaultSopsFile = ../../secrets/laptop-secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/var/lib/sops/keys.txt";
    age.generateKey = false;
    age.sshKeyPaths = [ ];
  };
  sops.secrets = {
    borg-home-password = { };
    borg-root-password = { };
    fedeizzo-path = {
      neededForUsers = true;
    };
    root-path = {
      neededForUsers = true;
    };

    xps-9510-wireguard-private-key = { };
  };
}

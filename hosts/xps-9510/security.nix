{ config, pkgs, username, syncthing, ... }:

{
  security.pam.services.lightdm.enableGnomeKeyring = false;
  security.pam.services.lightdm-greeters.enableGnomeKeyring = false;
  security.pam.services.login.fprintAuth = true;
  security.pam.services.system-local-login.fprintAuth = true;
  security.pam.services.lightdm.fprintAuth = true;
  security.pam.services.doas.fprintAuth = true;
  services.gnome.gnome-keyring.enable = false;
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
    defaultSopsFile = ../../secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/var/lib/sops/keys.txt";
    age.generateKey = false;
    age.sshKeyPaths = [ ];
  };
  sops.secrets = {
    # rasp-authkey = {
    #   owner = config.users.users.${username}.name;
    #   group = config.users.users.${username}.group;
    # };
    borg-home-password = { };
    borg-root-password = { };
    fedeizzo-path = {
      neededForUsers = true;
    };
    root-path = {
      neededForUsers = true;
    };
    personal-email-pass = {
      owner = username;
      sopsFile = ../../secrets/laptop-secrets.yaml;
      format = "yaml";
    };
    syncthing-private-key = {
      owner = username;
      sopsFile = ../../secrets/laptop-secrets.yaml;
      format = "yaml";
      path = "${syncthing.dataDir}/.config/syncthing/key.pem";
    };
  };
  sops.secrets.syncthing-public-key = {
    owner = username;
    mode = "0644";
    sopsFile = ../../secrets/laptop-secrets.yaml;
    format = "yaml";
    path = "${syncthing.dataDir}/.config/syncthing/cert.pem";
  };
}

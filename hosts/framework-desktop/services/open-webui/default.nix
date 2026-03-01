{ config, lib, ... }:

{
  services.open-webui = {
    enable = true;
    stateDir = "/persist/var/lib/open-webui";
    port = 5300;
    host = "0.0.0.0";
    openFirewall = false;
    environmentFile = config.sops.secrets.open-webui.path;
  };

  sops.secrets.open-webui = {
    format = "dotenv";
    mode = "0400";
    restartUnits = [
      "open-webui.service"
    ];
    sopsFile = ./open-webui-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
  # for consistent backup
  systemd.services.open-webui.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "open-webui";
    Group = "open-webui";

    StateDirectory = lib.mkForce "";
    WorkingDirectory = "/persist/var/lib/open-webui";
  };
}

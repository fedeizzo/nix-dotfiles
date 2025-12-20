{ config, ... }:

{
  services.open-webui = {
    enable = true;
    stateDir = "/var/lib/open-webui";
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
}

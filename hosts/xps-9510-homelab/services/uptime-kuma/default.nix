{ lib, ... }:

{
  services.uptime-kuma = {
    enable = true;
    appriseSupport = true;
    settings = {
      DATA_DIR = lib.mkForce "/var/lib/uptime-kuma";
      UPTIME_KUMA_PORT = lib.mkForce "45453";
    };
  };
}

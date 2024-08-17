{ ... }:

{
  services.prometheus = {
    enable = true;
    alertmanagers = [ ];
    exporters = {
      systemd.enable = true;
      wireguard.enable = true;
    };
    stateDir = "prometheus2";
  };
}

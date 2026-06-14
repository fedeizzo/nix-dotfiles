{
  flake.modules.nixos.influxdb = { ... }: {
    services.influxdb = {
      enable = true;
      dataDir = "/var/lib/influxdb";
    };
  };
}

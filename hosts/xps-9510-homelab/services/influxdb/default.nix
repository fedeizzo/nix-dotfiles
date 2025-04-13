{ ... }:

{
  services.influxdb = {
    enable = true;
    dataDir = "/var/lib/influxdb";
  };
}

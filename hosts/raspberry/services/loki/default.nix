{ ... }:

{
  services.loki = {
    enable = true;
    dataDir = "/var/volumes/loki";
    # configuration = { };
  };
}

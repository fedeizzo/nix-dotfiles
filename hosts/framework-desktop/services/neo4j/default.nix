{...}:

{
  services.neo4j = {
    enable = true;
    https.enable = false;
    http = {
      enable = true;
      listenAddress = ":7474";
    };
    directories.home = "/var/lib/neo4j";
  };
}
{ pkgs-unstable, ... }:

{
  services.neo4j = {
    enable = true;
    package = pkgs-unstable.neo4j;
    https.enable = false;
    http = {
      enable = true;
      listenAddress = ":7474";
    };
    directories.home = "/var/lib/neo4j";
    bolt.tlsLevel = "DISABLED";
    extraServerConfig = ''
      dbms.security.procedures.unrestricted=apoc.*,apoc.extended.*
      dbms.security.procedures.allowlist=apoc.*,apoc.extended.*
    '';
  };
}

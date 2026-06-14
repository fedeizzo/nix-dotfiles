{
  flake.modules.nixos.neo4j = { pkgs-unstable, ... }: {
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

    fi.services = [
      {
        name = "neo4j";
        port = 7474;
        dashboardSection = "Tools";
        toPersist = [
          {
            directory = "/var/lib/neo4j";
            user = "neo4j";
            group = "neo4j";
            mode = "u=rwx,g=,o=";
          }
        ];
        toBackup = [
          "/persist/var/lib/neo4j/data"
        ];
      }
    ];

    users.users.neo4j = {
      uid = 289;
      group = "neo4j";
    };
    users.groups.neo4j.gid = 289;
  };
}

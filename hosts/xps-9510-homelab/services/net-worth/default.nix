_:

{
  ############
  # DATABASE #
  ############
  # virtualisation.oci-containers.containers."net_worth_db" = {
  #   image = "postgres:16.0-alpine";
  #   autoStart = true;
  #   extraOptions = [ "--network=networth" ];
  #   ports = [ "9999:5432" ];
  #   environmentFiles = [ "/var/container_envs/net_worth_db" ];
  #   volumes = [
  #     "/var/volumes/net_worth_db:/var/lib/postgresql/data"
  #     "/etc/homelab/net_worth_db/init.sql:/docker-entrypoint-initdb.d/init.sql"
  #   ];
  # };
  # environment.etc.net_worth_db_init = {
  #   enable = true;
  #   source = ./init.sql;
  #   target = "homelab/net_worth_db/init.sql";
  # };

  ##############
  # DATA ENTRY #
  ##############
  virtualisation.oci-containers.containers."nocodb" = {
    image = "nocodb/nocodb";
    autoStart = true;
    extraOptions = [ "--network=networth" ];
    volumes = [ "/var/volumes/net_worth_nocodb:/usr/app/data" ];
    ports = [ "50003:8080" ];
  };

  # ###########
  # # GRAFANA #
  # ###########
  # virtualisation.oci-containers.containers."grafana" = {
  #   image = "grafana/grafana-oss";
  #   autoStart = true;
  #   user = "0";
  #   extraOptions = [ ];
  #   environmentFiles = [ "/var/container_envs/net_worth_grafana" ];
  #   volumes = [ "/var/volumes/net_worth_grafana:/var/lib/grafana" ];
  #   ports = [ "50002:3000" ];
  # };
}

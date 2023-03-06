{ pkgs, config, ... }:

{
  virtualisation.oci-containers.containers."fireflyiii-db" = {
    image = "mariadb";
    autoStart = true;
    environmentFiles = [ "/var/container_envs/fireflyiii-db" ];
    extraOptions = [ "--network=homelab" ];
    volumes = [ "/var/volumes/firefly-db:/var/lib/mysql" ];
  };
  virtualisation.oci-containers.containers."fireflyiii" = {
    image = "fireflyiii/core:version-v6.0";
    autoStart = true;
    extraOptions = [ "--network=homelab" ];
    dependsOn = [ "fireflyiii-db" ];
    environmentFiles = [
      "/var/container_envs/fireflyiii-db"
      "/var/container_envs/fireflyiii"
    ];
    volumes = [ "/var/volumes/firefly:/var/www/html/storage/upload" ];
  };
  virtualisation.oci-containers.containers."fireflyiii-importer" = {
    image = "fireflyiii/data-importer:latest";
    autoStart = true;
    extraOptions = [ "--network=homelab" ];
    ports = [ "8989:8080" ];
    dependsOn = [ "fireflyiii" ];
    environmentFiles = [ "/var/container_envs/fireflyiii-importer" ];
  };
}

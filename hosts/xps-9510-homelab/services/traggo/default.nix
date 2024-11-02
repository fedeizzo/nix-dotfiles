_:

{
  virtualisation.oci-containers.containers."traggo" = {
    image = "traggo/server:latest";
    autoStart = true;
    environmentFiles = [
      "/var/container_envs/traggo"
    ];
    extraOptions = [ "--memory=512m" ];
    ports = [ "50008:3030" ];
    volumes = [
      "/var/volumes/traggo:/opt/traggo/data"
    ];
  };
}

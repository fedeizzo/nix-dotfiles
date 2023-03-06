{ lib, config, ... }:

with lib;
with builtints;
{
  config = mkIf (elem "vaultwarden" config.containers) {
    virtualisation.oci-containers = {
      autoStart = false;
      backend = "podman";
      containers = {
        vaultwarden = {
          image = "vaultwarden/server:latest";
          # container
          extraOptions = [ "--restart=unless-stopped" ];
          volumes = [
            "/var/docker/vaultwarden/:/data/"
          ];
        };
      };
    };
  };
}

{ ... }:

{
  imports = [
    ./blocky
    ./traefik
    ./garmindb
    ./fedeizzo.dev
    ./net-worth
    # ./prometheus
    ./diun
  ];

  config = {
    virtualisation.oci-containers.backend = "docker";
    virtualisation = {
      docker = {
        enable = true;
        enableOnBoot = true;
        enableNvidia = false;
      };
      podman.enable = false;
    };
  };
}

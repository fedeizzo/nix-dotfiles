{ ... }:

{
  virtualisation.oci-containers.containers."garmindb-sync" = {
    image = "fedeizzo/garmindb-sync:latest";
    autoStart = true;
  };
}


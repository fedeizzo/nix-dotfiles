{}:

{
  virtualisation.oci-containers.containers."fedeizzodev" = {
    image = "fedeizzo/website:latest";
    autoStart = true;
    extraOptions = [ "--network=homelab" "--memory=512m" ];
    ports = [ "50001:80" ];
  };
}

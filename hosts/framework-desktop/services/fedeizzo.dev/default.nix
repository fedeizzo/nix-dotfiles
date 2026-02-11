_:

{
  virtualisation.oci-containers.containers."fedeizzodev" = {
    image = "fedeizzo/website:latest";
    autoStart = true;
    extraOptions = [ "--memory=512m" ];
    ports = [ "50001:80" ];
  };
}

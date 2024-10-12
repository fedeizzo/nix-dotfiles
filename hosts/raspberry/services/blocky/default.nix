_:

{
  environment.etc.blocky-config = {
    enable = true;
    source = ./blocky-config.yaml;
    target = "blocky/config.yaml";
  };

  virtualisation.oci-containers.containers."blocky" = {
    image = "spx01/blocky";
    ports = [ "53:53/udp" ];
    autoStart = true;
    extraOptions = [ ];
    environment = {
      TZ = "Europe/Paris";
    };
    volumes = [
      "/etc/blocky/config.yaml:/app/config.yml"
    ];
  };

}

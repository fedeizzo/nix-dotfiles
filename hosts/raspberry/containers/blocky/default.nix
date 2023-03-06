{ pkgs, config, ... }:

{
  environment.etc.blocky-config = {
    enable = true;
    source = ./config.yaml;
    target = "blocky/config.yaml";
  };
  virtualisation.oci-containers.containers."blocky" = {
    image = "spx01/blocky";
    autoStart = true;
    environment = {
      TZ = "Europe/Rome";
    };
    volumes = [ "/etc/blocky/config.yaml:/app/config.yml" ];
    extraOptions = [ "--memory=512Mi" ];
  };
}

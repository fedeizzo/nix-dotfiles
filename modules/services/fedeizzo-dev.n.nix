{
  flake.modules.nixos.fedeizzo-dev = {
    virtualisation.oci-containers.containers."fedeizzodev" = {
      image = "fedeizzo/website:latest";
      autoStart = true;
      extraOptions = [ "--memory=512m" ];
      ports = [ "50001:80" ];
    };

    fi.services = [
      { name = "fedeizzodev"; subdomain = null; port = 50001; isExposed = true; dashboardSection = "Exposed"; dashboardIcon = "hugo"; }
    ];
  };
}

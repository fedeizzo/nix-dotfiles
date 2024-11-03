{ hostname, ... }:

{
  services.sunshine = {
    enable = true;
    openFirewall = true;
    settings = {
      port = ""; # TODO choose
      general = {
        locale = "en";
        sunshine_name = "${hostname}";
        min_log_level = "info";
      };

      network = {
        upnp = "off";
      };
    };
  };
}

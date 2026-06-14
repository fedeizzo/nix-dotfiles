{
  flake.modules.nixos.x1-nano = {
    networking = {
      networkmanager = {
        dns = "none";
        wifi.powersave = true;
      };
      dhcpcd.enable = false;
      nameservers = [ "192.168.7.1" "1.1.1.1" ];
      firewall = {
        enable = true;
        allowedTCPPorts = [ 2300 ];
      };
      interfaces.wlp0s20f3 = { };
    };
  };
}

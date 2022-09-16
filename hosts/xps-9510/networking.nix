{ hostname, pkgs, inputs, ... }:

{
  networking.hostName = hostname;
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.firewall = {
    enable = true;
    checkReversePath = "loose";
  };
  services.tailscale.enable = true;
  environment.systemPackages = [ inputs.deploy-rs.defaultPackage.x86_64-linux ];
}

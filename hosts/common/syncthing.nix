{ pkgs, username, syncthing, config, ... }:

let
  devices = (builtins.listToAttrs
    (builtins.map (el: { name = el.name; value = el; }) syncthing.devices));
  deviceNames = (builtins.map (el: el.name) syncthing.devices);
in
{
  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 22000 ];
  environment.systemPackages = [ pkgs.syncthing ];
  services.syncthing = {
    enable = true;
    systemService = true;
    openDefaultPorts = false;
    overrideFolders = true;
    overrideDevices = true;
    relay.enable = false;
    # cert = config.sops.secrets.syncthing-public-key.path;
    # key = config.sops.secrets.syncthing-public-key.path;
    user = "${syncthing.user}";
    group = "keys"; # here the group is keys because synthing must have access to /run/secrets.d directory
    dataDir = "${syncthing.dataDir}";
    extraOptions = {
      gui = {
        theme = "black";
      };
      options = {
        globalAnnounceEnabled = false;
        localAnnounceEnabled = false;
        startBrowser = false;
      };
    };
    devices = devices;
    folders = {
      "University" = {
        enable = true;
        path = "/home/${syncthing.user}/uni";
        watch = true;
        type = (builtins.elemAt (builtins.filter (el: el.name == "University") syncthing.folders) 0).role;
        versioning = {
          type = "simple";
          params.keep = "10";
        };
        devices = deviceNames;
      };
      "nix-dotfiles" = {
        enable = true;
        path = "/home/${syncthing.user}/nix-dotfiles";
        watch = true;
        type = (builtins.elemAt (builtins.filter (el: el.name == "nix-dotfiles") syncthing.folders) 0).role;
        versioning = {
          type = "simple";
          params.keep = "10";
        };
        devices = deviceNames;
      };
      "videoFiles" = {
        enable = true;
        path = "/home/${syncthing.user}/docs/videoFiles";
        watch = true;
        type = (builtins.elemAt (builtins.filter (el: el.name == "videoFiles") syncthing.folders) 0).role;
        versioning = {
          type = "simple";
          params.keep = "10";
        };
        devices = deviceNames;
      };
    };
  };
}

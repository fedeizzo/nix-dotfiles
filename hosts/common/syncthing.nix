{ pkgs, username, syncthing, config, lib, ... }:

let
  # TODO fix deviceNames
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
    # group = "keys"; # here the group is keys because synthing must have access to /run/secrets.d directory
    group = "users";
    dataDir = "${syncthing.dataDir}";
    extraOptions = {
      gui = {
        theme = "black";
      };
      options = {
        globalAnnounceEnabled = false;
        localAnnounceEnabled = false;
        startBrowser = false;
        relaysEnabled = false;
      };
    };
    devices = devices;
    folders = {
      "University" = {
        enable = true;
        path = "${syncthing.dataDir}/uni";
        watch = true;
        type = (builtins.elemAt (builtins.filter (el: el.name == "University") syncthing.folders) 0).role;
        versioning = {
          type = "simple";
          params.keep = "3";
        };
        devices = (lib.lists.remove "smartphone" deviceNames);
      };
      "nix-dotfiles" = {
        enable = true;
        path = "${syncthing.dataDir}/nix-dotfiles";
        watch = true;
        type = (builtins.elemAt (builtins.filter (el: el.name == "nix-dotfiles") syncthing.folders) 0).role;
        versioning = {
          type = "simple";
          params.keep = "3";
        };
        devices = (lib.lists.remove "smartphone" deviceNames);
      };
      "videoFiles" = {
        enable = true;
        path = "${syncthing.dataDir}/docs/videoFiles";
        watch = true;
        type = (builtins.elemAt (builtins.filter (el: el.name == "videoFiles") syncthing.folders) 0).role;
        versioning = {
          type = "simple";
          params.keep = "3";
        };
        devices = (lib.lists.remove "smartphone" deviceNames);
      };
      "org" = {
        enable = true;
        path = "${syncthing.dataDir}/org";
        watch = true;
        type = (builtins.elemAt (builtins.filter (el: el.name == "org") syncthing.folders) 0).role;
        versioning = {
          type = "simple";
          params.keep = "3";
        };
        devices = deviceNames;
      };
    };
  };
}

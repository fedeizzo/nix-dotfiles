{ lib, config, ... }:

lib.mkIf (config.fs == "btrfs") {
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/nixos"
      "/var/lib/bluetooth"
      "/var/lib/cni"
      "/var/lib/containers"
      "/var/lib/libvirt"
      "/var/lib/docker"
      "/var/lib/fprint"
      "/var/lib/tailscale"
      "/var/log"
    ];
    files = [
      "/etc/adjtime"
      "/etc/NIXOS"
      "/etc/machine-id"
      "/var/lib/NetworkManager/secret_key"
      "/var/lib/NetworkManager/seen-bssids"
      "/var/lib/NetworkManager/timestamps"
    ];
  };
}

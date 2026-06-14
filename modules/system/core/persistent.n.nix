{
  flake.modules.nixos.persistent = { inputs, ... }: {
    imports = [ inputs.impermanence.nixosModules.impermanence ];
    environment.persistence."/persist" = {
      hideMounts = true;
      directories = [
        "/etc/NetworkManager/system-connections"
        "/etc/nixos"
        "/var/lib/nixos"
        "/var/lib/NetworkManager"
        "/var/lib/cni"
        "/var/lib/containers"
        "/var/lib/docker"
        "/root/.ssh"
        "/root/.cache"
        "/root/.local"
        "/var/lib/systemd/linger"
        "/var/lib/systemd/timers"
      ];
      files = [
        "/etc/adjtime"
        "/etc/machine-id"
      ];
    };
  };
}

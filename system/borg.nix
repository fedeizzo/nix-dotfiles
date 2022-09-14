{ config, pkgs, ... }:

let
  borgHomeRepo = "rasp@home-lab:~/backup/home-persistent-laptop";
  borgRootRepo = "rasp@home-lab:~/backup/root-persistent-laptop";
in
{
  services.borgbackup.jobs = {
    root-persistent = {
      user = "root";
      group = "root";
      repo = "${borgRootRepo}";
      environment = {
        BORG_RSH = "ssh -i /home/${config.username}/.ssh/id_rsa";
      };
      paths = [
        "/persist/etc"
        "/persist/root"
        "/persist/var/lib/NetworkManager"
        "/persist/var/lib/bluetooth"
        "/persist/var/lib/fprint"
        "/persist/var/lib/tailscale"
      ];
      compression = "zstd,3";
      archiveBaseName = "${config.networking.hostName}";
      doInit = true;
      preHook = ''
        echo "Check internet connection"
        is_network_down=1
        while [[ $is_network_down == 1 ]]; do
            ${pkgs.unixtools.ping}/bin/ping -c1 -q 8.8.8.8 > /dev/null
            is_network_down=$?
            sleep 10
        done
        echo "Start tailscale VPN"
        ${pkgs.tailscale}/bin/tailscale up
        echo "PreHook done"
      '';
      postCreate = ''
        echo "PostCreate done"
      '';
      startAt = [ "daily" ];
      persistentTimer = true;
      postPrune = "${pkgs.borgbackup}/bin/borg --progress compact ${borgRootRepo}";
      encryption = {
        passCommand = "cat ${config.sops.secrets.borg-root-password.path}";
        mode = "repokey-blake2";
      };
      prune = {
        prefix = "${config.networking.hostName}";
        keep = {
          daily = 5;
          weekly = 15;
          monthly = 45;
        };
      };
      extraPruneArgs = "--save-space --stats";
      extraCreateArgs = "--progress --stats";
    };
    home-persistent = {
      user = "root";
      group = "root";
      repo = "${borgHomeRepo}";
      environment = {
        BORG_RSH = "ssh -i /home/${config.username}/.ssh/id_rsa";
      };
      paths = [ "/persist/home/fedeizzo" ];
      exclude = [
        "/persist/home/fedeizzo/.cache"
        "/persist/home/fedeizzo/.cargo"
        "/persist/home/fedeizzo/**/venv"
        "/persist/home/fedeizzo/**/node_modules"
        "/persist/home/fedeizzo/**/.venv"
        "/persist/home/fedeizzo/**/pg_data"
        "/persist/home/fedeizzo/**/postgres_data"
        "/persist/home/fedeizzo/fbk/mappiamo/data"
      ];
      compression = "zstd,3";
      archiveBaseName = "${config.networking.hostName}";
      doInit = true;
      preHook = ''
        echo "Check internet connection"
        is_network_down=1
        while [[ $is_network_down == 1 ]]; do
            ${pkgs.unixtools.ping}/bin/ping -c1 -q 8.8.8.8 > /dev/null
            is_network_down=$?
            sleep 10
        done
        echo "Start tailscale VPN"
        ${pkgs.tailscale}/bin/tailscale up
        echo "PreHook done"
      '';
      postCreate = ''
        echo "PostCreate done"
      '';
      startAt = [ "daily" ];
      persistentTimer = true;
      postPrune = "${pkgs.borgbackup}/bin/borg --progress compact ${borgHomeRepo}";
      encryption = {
        passCommand = "cat ${config.sops.secrets.borg-home-password.path}";
        mode = "repokey-blake2";
      };
      prune = {
        prefix = "${config.networking.hostName}";
        keep = {
          daily = 5;
          weekly = 15;
          monthly = 45;
        };
      };
      extraPruneArgs = "--save-space --stats";
      extraCreateArgs = "--progress --stats";
    };
  };
}

{ inputs, pkgs, hostname, ... }:

{
  imports = [
    inputs.comin.nixosModules.comin
  ];

  services.comin = {
    enable = true;
    inherit hostname;
    exporter = {
      listen_address = "0.0.0.0";
      port = 40000;
    };
    remotes = [
      {
        name = "origin";
        url = "https://github.com/fedeizzo/nix-dotfiles.git";
        branches.testing.name = ""; # No testing branch on this remote
        branches.main.name = "master";
        poller.period = 60; # 60s
        timeout = 300; # 300s = 5m
      }
    ];
  };

  systemd.services.restart-docker-containers = {
    path = with pkgs; [ nixos-rebuild jq systemd ];

    script = ''
      currentGeneration=$(nixos-rebuild list-generations --json | jq --raw-output '.[] | select(.current == true) | .date')
      echo "Current generation: $currentGeneration"
      currentGenerationInSeconds=$(date -d "$currentGeneration" +"%s")
      echo "Current generation in seconds: $currentGenerationInSeconds"
      lastRestart=$(systemctl show --property=ActiveEnterTimestamp docker-fedeizzodev.service | cut -d= -f2)
      echo "Last restart: $lastRestart"
      lastRestartInSeconds=$(date -d "$lastRestart" +"%s")
      echo "Last restart in seconds: $lastRestartInSeconds"

      if [ "$currentGenerationInSeconds" -ge "$lastRestartInSeconds" ]; then
         echo "Restarting containers..."
         systemctl restart docker-*.service
         echo "Done."
      else
         echo "No restart required, already the last generation."
      fi
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "root";
    };
  };

  systemd.timers.restart-docker-containers = {
    enable = true;
    wantedBy = [ "timers.target" ];

    timerConfig = {
      OnCalendar = "*-*-* *:*:00";
      Unit = "restart-docker-containers.service";
    };
  };
}

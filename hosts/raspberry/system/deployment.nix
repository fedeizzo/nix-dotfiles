{ inputs, pkgs, ... }:

{
  imports = [
    inputs.comin.nixosModules.comin
  ];

  services.comin = {
    enable = true;
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
      echo "Last generation: "$currentGeneration
      currentGenerationInSeconds=$(date -d $currentGeneration +"%s")
      echo "Last generation in seconds: "$currentGenerationInSeconds
      now=$(date -u +"%s")

      echo "Now: "$now

      if [ $currentGenerationInSeconds -ge $now ]; then
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

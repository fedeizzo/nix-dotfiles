{ pkgs, ... }:

{
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

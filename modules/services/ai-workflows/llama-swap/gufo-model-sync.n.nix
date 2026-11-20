# Scheduled Gufo model sync: installs missing models and runs
# `gufo model update` on installed ones, daily via a systemd timer.
# Only the timer starts it; switches and boots never trigger downloads.
# The package has the lfm3 extension compiled in, so LFM3/d1 models are
# syncable alongside gufo's built-in ones.
# Each models/*.nix file declares its own services.gufo-model-sync.models
# entry next to the llama-swap model that serves it.
{
  flake.modules.nixos.llama-swap-gufo-model-sync = { inputs, pkgs, ... }:
    {
      imports = [
        inputs.gufo.nixosModules.model-sync
        inputs.self.modules.nixos.llama-swap-gufo-lfm3
      ];

      services.gufo-model-sync = {
        enable = true;
        package = pkgs.gufo-lfm3;
        onCalendar = "daily";
        randomizedDelaySec = "30m";
      };

      # Models are re-downloadable, so persist without backup.
      fi.services = [
        {
          name = "gufo-model-sync";
          dashboardSection = "Tools";
          shouldBehindReverseProxy = false;
          shouldMonitorUptime = false;
          shouldBeInDashboard = false;
          toPersist = [
            {
              directory = "/var/lib/gufo";
              user = "gufo";
              group = "gufo";
              mode = "u=rwx,g=rx,o=";
            }
          ];
        }
      ];

      # llama-swap keeps its DynamicUser; the gufo group (sync UMask 0027)
      # grants it read access to the synchronized models.
      systemd.services.llama-swap.serviceConfig = {
        SupplementaryGroups = [ "gufo" ];
        ReadOnlyPaths = [ "/var/lib/gufo/models" ];
      };
    };
}

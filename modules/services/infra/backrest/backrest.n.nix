{
  flake.modules.nixos.backrest = { pkgs, lib, ... }: {
    environment.systemPackages = [ pkgs.backrest ];
    systemd.services.backrest = {
      description = "Backrest";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        User = "root";
        ExecStart = lib.getExe pkgs.backrest;
      };
      environment = {
        BACKREST_PORT = "0.0.0.0:9898";
        BACKREST_CONFIG = "/root/.config/backrest/config.json";
        BACKREST_DATA = "/root/.local/backrest";
        XDG_CONFIG_HOME = "/root/.config";
        XDG_DATA_HOME = "/root/.local/share";
        XDG_CACHE_HOME = "/root/.cache";
      };
    };

    fi.services = [
      {
        name = "backrest";
        port = 9898;
        dashboardSection = "Tools";
        authType = "proxy";
        toPersist = [
          {
            directory = "/root/.config/backrest";
            user = "root";
            group = "root";
            mode = "u=rwx,g=,o=";
          }
          {
            directory = "/root/.local/backrest";
            user = "root";
            group = "root";
            mode = "u=rwx,g=,o=";
          }
        ];
        toBackup = [
          "/persist/root/.config/backrest"
        ];
      }
    ];
  };
}

{
  flake-file.inputs.comin = {
    url = "github:fedeizzo/comin/fix-ssh-commit-payload";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.modules.nixos.comin = { inputs, config, lib, pkgs, ... }: {
    imports = [ inputs.comin.nixosModules.comin ];

    services.comin = {
      enable = true;
      package = inputs.comin.packages.${pkgs.stdenv.hostPlatform.system}.comin;
      hostname = "homelab";
      debug = false;
      evalTimeout = 600;
      buildTimeout = 7200;
      exporter = {
        listen_address = "127.0.0.1";
        openFirewall = false;
      };
      remotes = [
        {
          name = "origin";
          url = "https://github.com/fedeizzo/nix-dotfiles.git";
          branches = {
            main = {
              name = "master";
              operation = "switch";
            };
            testing.name = "";
          };
          poller.period = 60;
        }
      ];
    };

    fi.services = lib.optionals config.services.comin.enable [
      {
        name = "comin";
        port = config.services.comin.exporter.port;
        dashboardSection = "Tools";
        shouldBehindReverseProxy = false;
        shouldMonitorUptime = false;
        shouldBeInDashboard = false;
        toPersist = [
          {
            directory = "/var/lib/comin";
            user = "root";
            group = "root";
            mode = "u=rwx,g=,o=";
          }
        ];
        toBackup = [ "/persist/var/lib/comin" ];
      }
    ];

    services.prometheus.scrapeConfigs = lib.mkIf
      (config.services.comin.enable && config.services.prometheus.enable) [
      {
        job_name = "comin";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.comin.exporter.port}" ];
        }];
      }
    ];
  };
}

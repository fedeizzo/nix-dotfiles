{
  flake.modules.nixos.silverbullet = { config, lib, pkgs, ... }: {
    services.silverbullet = {
      enable = true;
      listenAddress = "127.0.0.1";
      listenPort = 35557;
      spaceDir = "/var/lib/nextcloud/data/fedeizzo/files/wiki";
      user = "nextcloud";
      group = "nextcloud";
    };

    systemd.services.silverbullet = {
      preStart = lib.mkForce "${pkgs.coreutils}/bin/mkdir -p '${config.services.silverbullet.spaceDir}'";
      serviceConfig = {
        StateDirectory = lib.mkForce "";
      };
    };

    fi.services = [
      {
        name = "silverbullet";
        subdomain = "notes";
        port = config.services.silverbullet.listenPort;
        dashboardSection = "Tools";
        toPersist = [
          {
            directory = config.services.silverbullet.spaceDir;
            user = "nextcloud";
            group = "nextcloud";
            mode = "u=rwx,g=rx,o=";
          }
        ];
        toBackup = [
          "/persist${config.services.silverbullet.spaceDir}"
        ];
      }
    ];
  };
}

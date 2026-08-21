{
  flake.modules.nixos.silverbullet = { config, lib, pkgs, ... }: {
    services.silverbullet = {
      enable = true;
      listenAddress = "127.0.0.1";
      listenPort = 35557;
      spaceDir = "/var/lib/nextcloud/data/fedeizzo/files/wiki";
      user = "silverbullet";
      group = "silverbullet";
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
            user = "silverbullet";
            group = "silverbullet";
            mode = "u=rwx,g=rx,o=";
          }
        ];
        toBackup = [
          "/persist${config.services.silverbullet.spaceDir}"
        ];
      }
    ];

    users.users.silverbullet = {
      uid = 984;
      group = "silverbullet";
      extraGroups = [ "nextcloud" ];
    };
    users.groups.silverbullet.gid = 977;
  };
}

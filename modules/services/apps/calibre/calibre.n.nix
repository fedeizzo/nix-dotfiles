{
  flake.modules.nixos.calibre = { config, ... }: {
    services.calibre-web = {
      enable = true;
      user = "calibre-server";
      group = "calibre-server";
      listen.port = 44533;
      openFirewall = true;
      options = {
        calibreLibrary = "/var/lib/calibre-web";
        enableBookConversion = true;
        enableBookUploading = true;
        enableKepubify = true;
      };
    };

    fi.services = [
      {
        name = "calibre";
        port = config.services.calibre-web.listen.port;
        dashboardSection = "Media";
        toPersist = [
          {
            directory = config.services.calibre-web.options.calibreLibrary;
            user = "calibre-server";
            group = "calibre-server";
            mode = "u=rwx,g=rx,o=rx";
          }
        ];
        toBackup = [
          "/persist${config.services.calibre-web.options.calibreLibrary}"
        ];
      }
    ];

    users.users.calibre-server = {
      uid = 206;
      group = "calibre-server";
    };
    users.groups.calibre-server.gid = 206;
  };
}

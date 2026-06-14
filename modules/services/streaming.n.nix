{
  flake.modules.nixos.streaming = { pkgs, config, lib, ... }: {
    environment.systemPackages = with pkgs; [
      jellyfin
      jellyfin-web
      jellyfin-ffmpeg
    ];

    services = {
      jellyfin = {
        enable = true;
        openFirewall = true;
        user = "media";
        group = "media";
      };
      seerr.enable = true; # *rr integration in jellyfin
      radarr = {
        enable = true; # movies
        user = "media";
        group = "media";
      };
      sonarr = {
        enable = true; # series
        user = "media";
        group = "media";
      };
      prowlarr.enable = true; # tracker
      bazarr = {
        enable = true; # subtitles
        user = "media";
        group = "media";
      };

      deluge = {
        enable = true;
        user = "media";
        group = "media";
        web.enable = true;
        dataDir = "/games/jellyfin/torrent";
      };
    };
    systemd.services.sonarr.serviceConfig.ProtectHome = lib.mkForce false;
    systemd.services.radarr.serviceConfig.ProtectHome = lib.mkForce false;

    nixpkgs.overlays = with pkgs; [
      (
        final: prev:
          {
            jellyfin-web = prev.jellyfin-web.overrideAttrs (finalAttrs: previousAttrs: {
              installPhase = ''
                runHook preInstall

                # this is the important line
                sed -i "s#</head>#<script src=\"configurationpage?name=skip-intro-button.js\"></script></head>#" dist/index.html

                mkdir -p $out/share
                cp -a dist $out/share/jellyfin-web

                runHook postInstall
              '';
            });
          }
      )
    ];

    virtualisation.oci-containers.containers."flaresolverr" = {
      image = "ghcr.io/flaresolverr/flaresolverr:latest";
      autoStart = true;
      ports = [ "8191:8191" ];

      environment = {
        LOG_LEVEL = "info";
      };
    };

    fi.services = [
      {
        name = "jellyfin";
        port = 8096;
        isExposed = true;
        dashboardSection = "Exposed media";
        toPersist = [
          {
            directory = config.services.jellyfin.dataDir;
            user = "media";
            group = "media";
            mode = "u=rwx,g=,o=";
          }
        ];
        toBackup = [
          "/persist${config.services.jellyfin.dataDir}"
        ];
      }
      {
        name = "jellyseerr";
        port = 5055;
        isExposed = true;
        dashboardSection = "Exposed media";
        toPersist = [
          {
            directory = "/var/lib/private/jellyseerr";
            user = "jellyseerr";
            group = "jellyseerr";
            mode = "u=rwx,g=rx,o=rx";
          }
        ];
        toBackup = [
          "/persist/var/lib/private/jellyseerr"
        ];
      }
      {
        name = "sonarr"; inherit (config.services.sonarr.settings.server) port; dashboardSection = "Media";
        toPersist = [
          {
            directory = config.services.sonarr.dataDir;
            user = "media";
            group = "media";
            mode = "u=rwx,g=rx,o=rx";
          }
        ];
        toBackup = [
          "/persist${config.services.sonarr.dataDir}"
        ];
      }
      {
        name = "radarr"; inherit (config.services.radarr.settings.server) port; dashboardSection = "Media";
        toPersist = [
          {
            directory = config.services.radarr.dataDir;
            user = "media";
            group = "media";
            mode = "u=rwx,g=rx,o=rx";
          }
        ];
        toBackup = [
          "/persist${config.services.radarr.dataDir}"
        ];
      }
      {
        name = "prowlarr"; inherit (config.services.prowlarr.settings.server) port; dashboardSection = "Media";
        toPersist = [
          {
            directory = "/var/lib/private/prowlarr";
            user = "prowlarr";
            group = "prowlarr";
            mode = "u=rwx,g=rx,o=rx";
          }
        ];
        toBackup = [
          "/persist/var/lib/private/prowlarr"
        ];
      }
      {
        name = "deluge"; inherit (config.services.deluge.web) port; dashboardSection = "Media";
        toBackup = [
          "${config.services.deluge.dataDir}/.config"
        ];
      }
      {
        name = "bazarr";
        port = config.services.bazarr.listenPort;
        dashboardSection = "Media";
        toPersist = [
          {
            directory = "/var/lib/bazarr";
            user = "media";
            group = "media";
            mode = "u=rwx,g=rx,o=rx";
          }
        ];
        toBackup = [
          "/persist/var/lib/bazarr"
        ];
      }
    ];

    # Consistent backup IDs across machines
    users.users = {
      media = {
        uid = 800;
        home = "/home/media";
        isSystemUser = true;
        group = "media";
      };
      prowlarr = {
        uid = 61654;
        group = "prowlarr";
        isSystemUser = true;
      };
      jellyseerr = {
        uid = 62900;
        group = "jellyseerr";
        isSystemUser = true;
      };
    };

    users.groups = {
      media.gid = 1800;
      prowlarr.gid = 61654;
      jellyseerr.gid = 62900;
    };
  };
}

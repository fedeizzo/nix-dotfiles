{ pkgs, ... }:

{
  # media group to be used by each service
  users.groups.media = {
    gid = 1800;
  };
  users.users.media = {
    uid = 800;
    home = "/home/media";
    isSystemUser = true;
    group = "media";
  };

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
    jellyseerr.enable = true; # *rr integration in jellyfin
    # jellyseerr.package = pkgs-unstable.jellyseerr; # *rr integration in jellyfin
    radarr.enable = true; # movies
    radarr.user = "media";
    radarr.group = "media";
    sonarr.enable = true; # series
    sonarr.user = "media";
    sonarr.group = "media";
    prowlarr.enable = true; # tracker
    bazarr.enable = true; # subtitles
    bazarr.user = "media";
    bazarr.group = "media";
    # flaresolverr.enable = true; # bypass cloudflare
    # flaresolverr.package = pkgs.nur.repos.xddxdd.flaresolverr-21hsmw;

    deluge = {
      enable = true;
      user = "media";
      group = "media";
      web.enable = true;
      dataDir = "/games/jellyfin/torrent";
    };
  };
  
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
}

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
  users.users.prowlarr.uid = 61654; # make backup consistent across machines
  users.groups.prowlarr.gid = 61654; # make backup consistent across machines
  users.users.prowlarr.group = "prowlarr";
  users.users.prowlarr.isSystemUser = true;
  users.users.jellyseerr.uid = 62900; # make backup consistent across machines
  users.users.jellyseerr.isSystemUser = true;
  users.groups.jellyseerr.gid = 62900; # make backup consistent across machines
  users.users.jellyseerr.group = "jellyseerr";

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

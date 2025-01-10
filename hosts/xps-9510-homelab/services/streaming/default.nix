{ pkgs, ... }:

{
  # media group to be used by each service
  users.groups.media = {
    gid = 1800;
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
      group = "media";
    };
    jellyseerr.enable = true; # *rr integration in jellyfin
    # jellyseerr.package = pkgs-unstable.jellyseerr; # *rr integration in jellyfin
    radarr.enable = true; # movies
    radarr.group = "media";
    sonarr.enable = true; # series
    sonarr.group = "media";
    prowlarr.enable = true; # tracker
    bazarr.enable = true; # subtitles
    bazarr.group = "media";
    # flaresolverr.enable = true; # bypass cloudflare
    # flaresolverr.package = pkgs.nur.repos.xddxdd.flaresolverr-21hsmw;

    deluge = {
      enable = true;
      group = "media";
      web.enable = true;
      dataDir = "/games/jellyfin/torrent";
    };
  };

  virtualisation.oci-containers.containers."flaresolverr" = {
    image = "ghcr.io/flaresolverr/flaresolverr:latest";
    autoStart = true;
    ports = [ "8191:8191" ];

    environment = {
      LOG_LEVEL = "info";
    };
  };
}

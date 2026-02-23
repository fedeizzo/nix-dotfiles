{ ... }:

{
  # make backup consistent across machines
  users.users = {
    calibre-server.uid = 206;
    calibre-server.group = "calibre-server";

    grafana.uid = 196;
    grafana.group = "grafana";
    loki.uid = 994;
    promtail.uid = 992;

    hass.uid = 286;
    hass.group = "hass";
    mosquitto.uid = 246;
    mosquitto.group = "mosquitto";
    zigbee2mqtt.uid = 317;
    zigbee2mqtt.group = "zigbee2mqtt";

    immich.uid = 983;
    immich.group = "immich";

    nextcloud.uid = 975;
    nextcloud.group = "nextcloud";

    opencloud.uid = 997;
    opencloud.group = "opencloud";

    paperless.uid = 315;
    paperless.group = "paperless";

    postgres.uid = 71;
    postgres.group = "postgres";

    traefik.uid = 990;
    traefik.group = "traefik";

    garmindb = {
      uid = 970;
      isSystemUser = true;
      group = "garmindb";
      home = "/var/lib/garmindb";
    };

    media = {
      uid = 800;
      home = "/home/media";
      isSystemUser = true;
      group = "media";
    };
    prowlarr.uid = 61654;
    prowlarr.group = "prowlarr";
    prowlarr.isSystemUser = true;
    jellyseerr.uid = 62900;
    jellyseerr.isSystemUser = true;
    jellyseerr.group = "jellyseerr";
  };
  users.groups = {
    calibre-server.gid = 206;

    grafana.gid = 992;

    hass.gid = 286;
    mosquitto.gid = 246;
    zigbee2mqtt.gid = 317;

    immich.gid = 976;

    nextcloud.gid = 969;

    opencloud.gid = 456;

    paperless.gid = 315;

    postgres.gid = 71;

    traefik.gid = 989;

    garmindb.gid = 962;

    media.gid = 1800;
    prowlarr.gid = 61654;
    jellyseerr.gid = 62900;
  };

}

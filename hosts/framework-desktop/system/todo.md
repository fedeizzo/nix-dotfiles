```
users.groups.jellyfin = { gid = 911; };
users.users.jellyfin = {
  uid = 911;
  group = "jellyfin";
  home = "/var/lib/jellyfin";
  isSystemUser = true;
};

users.groups.radarr = { gid = 1001; };
users.users.radarr = {
  uid = 1001;
  group = "radarr";
  home = "/var/lib/radarr";
  isSystemUser = true;
};

# Repeat for sonarr, immich, paperless, etc.

```

```
  { directory = "/var/lib/colord"; user = "colord"; group = "colord"; mode = "u=rwx,g=rx,o="; }
  { directory = "/var/lib/another-service"; user = "service"; group = "service"; mode = "u=rwx,g=rx,o="; }
```

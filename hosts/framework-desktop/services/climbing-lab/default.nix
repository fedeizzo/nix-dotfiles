{ config, ... }:

{
  services.tindeq-exporter = {
    enable = true;
    user = "nextcloud";
    group = "nextcloud";
    watchDirectory = "${config.services.nextcloud.home}/data/fedeizzo/files/tindeq-exports";
    databaseDirectory = "/var/lib/tindeq";
    deleteAfterImport = false;
  };
}

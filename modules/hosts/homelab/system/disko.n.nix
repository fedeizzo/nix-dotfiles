{
  flake.modules.nixos.homelab = {
    disko.devices.disk.main.content.partitions.luks.content.content.subvolumes = {
      "/games" = {
        mountpoint = "/games";
        mountOptions = [
          "subvol=games"
          "noatime"
          "compress=zstd:3"
          "ssd"
          "space_cache=v2"
        ];
      };

      "/home/media" = {
        mountpoint = "/home/media";
        mountOptions = [
          "subvol=home/media"
          "noatime"
          "compress=zstd:3"
          "ssd"
          "space_cache=v2"
        ];
      };
    };
  };
}

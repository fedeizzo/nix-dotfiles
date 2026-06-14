{
  flake.modules.nixos.disk = {
    services = {
      smartd = {
        enable = true;
        autodetect = true;
        notifications.wall.enable = true;
      };
      fstrim = {
        enable = true;
        interval = "weekly";
      };
      udisks2 = {
        enable = true;
        mountOnMedia = true;
      };
    };
  };
}

{ ... }:

{
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
  };

  services.udisks2 = {
    # automount external disks
    enable = true;
    mountOnMedia = true;
  };
}

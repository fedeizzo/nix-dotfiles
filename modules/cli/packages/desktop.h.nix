{
  flake.modules.homeManager.cli-desktop = { pkgs, lib, ... }: {
    home.packages = lib.optionals pkgs.stdenv.isLinux (with pkgs; [
      dragon-drop
      (makeDesktopItem {
        name = "reboot";
        exec = "reboot";
        comment = "Reboot the system";
        desktopName = "reboot";
        type = "Application";
      })
      (makeDesktopItem {
        name = "shutdown";
        exec = "shutdown now";
        comment = "Shutdown the system";
        desktopName = "shutdown";
        type = "Application";
      })
    ]);
  };
}

{
  flake.modules.nixos.boot = { pkgs, ... }: {
    boot = {
      supportedFilesystems = [ "ext4" "btrfs" ];
      loader.systemd-boot.enable = true;
      tmp.cleanOnBoot = true;
      kernelPackages = pkgs.linuxPackages_latest;
      extraModulePackages = [ ];
      consoleLogLevel = 0;
    };
  };
}

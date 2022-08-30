{ config, lib, pkgs, nixpkgs-unstable, ... }:

{
  powerManagement.enable = true;
  hardware.opengl = {
    driSupport32Bit = true;
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver
      pkgs.vaapiIntel
      pkgs.vaapiVdpau
      pkgs.libvdpau-va-gl
    ];
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    disabledPlugins = [ "sap" ];
  };
  hardware.cpu.intel.updateMicrocode = true;
  sound = {
    enable = false;
    mediaKeys.enable = false;
  };
  services.smartd = {
    enable = true;
    autodetect = true;
    notifications.x11.enable = true;
  };
  # keyboard daemon for remapping
  systemd.services.keyd = {
    enable = true;
    description = "key remapping daemon";
    script = "${nixpkgs-unstable.keyd}/bin/keyd";
    requires = [ "local-fs.target" ];
    after = [ "local-fs.target" ];
    serviceConfig = {
      Type = "simple";
    };
    wantedBy = [ "sysinit.target" ];
  };
  environment.etc = {
    keyd = {
      target = "keyd/laptop-keyboard.conf";
      text = ''
        [ids]
        0001:0001
        [main]
        capslock = overload(control, esc)
      '';
    };
  };
}

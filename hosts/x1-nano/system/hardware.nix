{ inputs, pkgs, ... }:

{
  hardware = {
    trackpoint = {
      enable = true;
      speed = 200;
      sensitivity = 200;
      emulateWheel = true;
    };
    cpu.intel.updateMicrocode = true;
    i2c.enable = true; # external monitor control
    uinput.enable = true;
    logitech = {
      wireless.enable = true;
      wireless.enableGraphical = true;
    };
  };

  services = {
    # illum.enable = true; # Enable the brightness buttons
    pcscd.enable = true;
    fwupd.enable = true; # update firmware
  };

  zramSwap.enable = true;

  # https://nicholaslyz.com/blog/2024/04/29/how-to-undervolt-a-laptop-with-nixos/
  # services.undervolt = {
  #   enable = true;
  #   coreOffset = -90;
  #   uncoreOffset = -90;
  #   gpuOffset = -70;
  #   turbo = 0; # enabled
  #   p1.limit = 20;
  #   p1.window = 20;
  #   p2.limit = 40;
  #   p2.window = 10;
  # };

  systemd.services = {
    hdaverb-jack-fix-boot = {
      description = "Thinkpad X1 Nano: Fix crackly audio with headphones";
      wantedBy = [ "multi-user.target" ];
      after = [ "sys-devices-pci0000:00-0000:00:1f.3-skl_hda_dsp_generic-sound-card0-controlC0.device" ];
      requires = [ "sys-devices-pci0000:00-0000:00:1f.3-skl_hda_dsp_generic-sound-card0-controlC0.device" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.alsa-tools}/bin/hda-verb /dev/snd/hwC0D0 0x1d SET_PIN_WIDGET_CONTROL 0x0";
      };
    };
    hdaverb-jack-fix-resume = {
      description = "Thinkpad X1 Nano: Fix crackly audio with headphones";
      wantedBy = [ "post-resume.target" ];
      after = [ "sys-devices-pci0000:00-0000:00:1f.3-skl_hda_dsp_generic-sound-card0-controlC0.device" "post-resume.target" ];
      requires = [ "sys-devices-pci0000:00-0000:00:1f.3-skl_hda_dsp_generic-sound-card0-controlC0.device" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.alsa-tools}/bin/hda-verb /dev/snd/hwC0D0 0x1d SET_PIN_WIDGET_CONTROL 0x0";
      };
    };
  };

  services.solaar = {
    enable = true; # Enable the service
    package = pkgs.solaar; # The package to use
    window = "hide"; # Show the window on startup (show, *hide*, only [window only])
    batteryIcons = "solaar"; # Which battery icons to use (*regular*, symbolic, solaar)
    extraArgs = ""; # Extra arguments to pass to solaar on startup
  };

  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-nano
    inputs.solaar.nixosModules.default
  ];
}

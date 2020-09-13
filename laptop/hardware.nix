{ config, pkgs, ... }:

{
  # TODO maybe useful for Italian accent
  # services.xserver.xkbOptions = "eurosign:e";
  # TODO this 32bit support is useful only with steam
  # hardware.opengl.driSupport32Bit = true;
  # hardware.pulseaudio.support32Bit = true;

  hardware.opengl = {
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
  };

  hardware.bumblebee = {
    enable = true;
    driver = "nvidia";
  };

  hardware.cpu.intel.updateMicrocode = true;

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    # extraConfig = "load-module module-bluetooth-discover a2dp_config=\"ldac_eqmid=sq\"\n";
    extraConfig = ".fail
load-module module-device-restore
load-module module-stream-restore restore_device=false
load-module module-card-restore
load-module module-augment-properties
load-module module-switch-on-port-available
.ifexists module-udev-detect.so
load-module module-udev-detect
.else
load-module module-detect
.endif
.ifexists module-jackdbus-detect.so
.nofail
load-module module-jackdbus-detect channels=2
.fail
.endif
.ifexists module-bluetooth-policy.so
load-module module-bluetooth-policy
.endif
.ifexists module-bluetooth-discover.so
load-module module-bluetooth-discover a2dp_config="ldac_eqmid=sq"
.endif
load-module module-dbus-protocol
.ifexists module-esound-protocol-unix.so
load-module module-esound-protocol-unix
.endif
load-module module-native-protocol-unix
.ifexists module-gsettings.so
.nofail
load-module module-gsettings
.fail
.endif
load-module module-default-device-restore
load-module module-rescue-streams
load-module module-always-sink
load-module module-intended-roles
load-module module-suspend-on-idle
.ifexists module-console-kit.so
load-module module-console-kit
.endif
.ifexists module-systemd-login.so
load-module module-systemd-login
.endif
load-module module-position-event-sounds
load-module module-role-cork
load-module module-filter-heuristics
load-module module-filter-apply";
  };
}

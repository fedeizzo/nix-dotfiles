{ pkgs, modulesPath, inputs, lib, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
    (modulesPath + "/installer/scan/not-detected.nix")
  ];


  fileSystems = {
    "/" =
      {
        device = "/dev/disk/by-label/root";
        fsType = "ext4";
      };

    "/boot" =
      {
        device = "/dev/disk/by-label/boot";
        fsType = "vfat";
      };

    "/backup" =
      {
        device = "/dev/disk/by-label/backup";
        fsType = "ext4";
      };
  };

  swapDevices = [ ];

  # Required for the Wireless firmware
  hardware.enableRedistributableFirmware = true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  # add gpio group
  users.groups.gpio = { };

  # udev rule for gpio
  services.udev.extraRules = ''
    SUBSYSTEM=="bcm2835-gpiomem", KERNEL=="gpiomem", GROUP="gpio",MODE="0660"
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", ACTION=="add", RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio  /sys/class/gpio/export /sys/class/gpio/unexport ; chmod 220 /sys/class/gpio/export /sys/class/gpio/unexport'"
    SUBSYSTEM=="gpio", KERNEL=="gpio*", ACTION=="add",RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value ; chmod 660 /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value'"
  '';
  systemd.services = {
    fan-control = {
      enable = true;
      script = ''
        while true; do
          ontemp=55
          temp=$(${pkgs.libraspberrypi}/bin/vcgencmd measure_temp | egrep -o '[0-9]*\.[0-9]*')
          temp0=$${temp%.*}

          if [[ $temp > $ontemp ]]; then
              ${pkgs.libgpiod}/bin/gpioset --toggle 0 --chip gpiochip0 14=1
          else
              ${pkgs.libgpiod}/bin/gpioset --toggle 0 --chip gpiochip0 14=0
          fi
          sleep 10
        done
      '';
      unitConfig = {
        Type = "simple";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}

{ pkgs, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_rpi4;
    tmp.useTmpfs = true;
    tmp.cleanOnBoot = true;
    # ttyAMA0 is the serial console broken out to the GPIO
    kernelParams = [
      "8250.nr_uarts=1"
      "console=ttyAMA0,115200"
      "console=tty1"
      # A lot GUI programs need this, nearly all wayland applications
      "cma=128M"
      "cgroup_memory=1"
      "cgroup_enable=memory"
    ];
    loader = {
      # raspberryPi = {
      #   enable = true;
      #   uboot.enable = false;
      #   version = 4;
      #   firmwareConfig = ''
      #     dtparam=sd_poll_once=on
      #     dtoverlay=gpio-fan,gpiopin=14,temp=60000
      #   '';
      # };
      grub.enable = false;
      systemd-boot.enable = false;
      generic-extlinux-compatible.enable = false;
    };
    kernel.sysctl."net.ipv4.ip_forward" = 1;
    kernel.sysctl."net.ipv6.conf.all.forwarding" = 1;

    initrd.availableKernelModules = [ "reset-raspberrypi" "xhci_pci" "usbhid" "uas" ];
    initrd.kernelModules = [ ];
    kernelModules = [ ];
    extraModulePackages = [ ];
  };
}

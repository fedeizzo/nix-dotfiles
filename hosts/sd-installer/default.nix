{ raspberry-pi-4-hardware, pkgs, lib, freezer-config-to-cache, ... }:

{
  imports = [
    raspberry-pi-4-hardware
  ];

  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "pikvm-installer";

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.PermitRootLogin = "yes";
  };

  users.users.root = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILj7IsDH+Zjvb8wx22OkYxFtS6u4CssIkFQ3S8xtCVkz federico@fedeizzo.dev"
    ];
  };

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];

    # LIMIT I/O CONCURRENCY: Prevents Nix from overwhelming the USB bus.
    # By limiting jobs to 1, we ensure the system stays responsive.
    max-jobs = 1;
    cores = 4; # You can use all cores, but only on 1 job at a time.

    # DO NOT enable auto-optimise-store on USB storage;
    # the random I/O for hard-linking will cause hangs.
    auto-optimise-store = false;

    # Trust the binary cache to avoid local builds as much as possible.
    substituters = [ "https://cache.nixos.org" ];
    trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
  };

  nixpkgs.hostPlatform = "aarch64-linux";
  system.stateVersion = "25.11";

  # todo: remove this when this is fixed: https://github.com/NixOS/nixpkgs/issues/154163
  # related: https://github.com/NixOS/nixpkgs/issues/109280
  # related: https://discourse.nixos.org/t/cannot-build-raspberry-pi-sdimage-module-dw-hdmi-not-found/71804
  hardware.enableAllHardware = lib.mkForce false;


  boot.tmp.useTmpfs = true;
  boot.tmp.tmpfsSize = "4G";
  boot.kernelParams = [
    "usb-storage.quirks=0951:16a5:u" # Replace XXXX:YYYY with your stick's VID:PID from `lsusb`; forces usb-storage if UAS buggy
  ];
  zramSwap.enable = true;

  environment.systemPackages = with pkgs; [
    vim
    git
    htop # Great for watching RAM/CPU during builds
    pciutils
    usbutils
  ];
}

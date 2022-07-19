{ config, pkgs, lib, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_rpi4;
    tmpOnTmpfs = true;
    cleanTmpDir = true;
    # ttyAMA0 is the serial console broken out to the GPIO
    kernelParams = [
      "8250.nr_uarts=1"
      "console=ttyAMA0,115200"
      "console=tty1"
      # A lot GUI programs need this, nearly all wayland applications
      "cma=128M"
    ];
    loader = {
      raspberryPi.firmwareConfig = "dtparam=sd_poll_once=on";
      # raspberryPi = {
      #   enable = true;
      #   version = 4;
      # };
      grub.enable = false;
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = false;
    };
  };

  # Required for the Wireless firmware
  hardware.enableRedistributableFirmware = true;

  # NETWORKING
  networking = {
    hostName = "rasp-nixos"; # Define your hostname.
    networkmanager = {
      enable = true;
    };
  };

  # KEYMAP AND TIME
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    keyMap = "us";
  };
  time.timeZone = "Europe/Rome";
  time.hardwareClockInLocalTime = true;

  # PACKAGES
  environment.systemPackages = with pkgs; [
    bc
    curl
    killall
    wget
    git
    vim
    docker-compose
  ];
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
      enableNvidia = false;
    };
  };
  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
  };

  # SECURITY
  security.doas = {
    enable = true;
    extraRules = [
      { groups = [ "wheel" ]; keepEnv = true; persist = true; }
    ];
  };

  # USER
  users.users.rasp = {
    name = "rasp";
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "docker"
      "autologin"
      "users"
      "networkmanager"
    ];
    shell = pkgs.bash;
  };

  # NIX STUFF
  nixpkgs.config = {
    allowUnfree = true;
  };
  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    # Free up to 1GiB whenever there is less than 100MiB left.
    extraOptions = ''
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
      experimental-features = nix-command flakes
    '';
  };
  system.stateVersion = "22.05";
}

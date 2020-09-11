# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      ./environment.nix
      ./hardware-configuration.nix
      ./networking.nix
      ./programs.nix
      ./security.nix
      ./services.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.cleanTmpDir = true;

  # TODO see postBootCommand to do snapshots
  # boot.kernelPackages = pkgs.linuxPackages_latest;  # use latest kernel stable release

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # TODO see console for more infos
  # TODO see container in order to run copy of nixOs for steam and other pkg
  console = {
  #  font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Europe/Rome";

  # Enable sound.
  sound.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.xkbOptions = "eurosign:e";
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  # TODO see all options for bluetooth (LDAC headset ecc.)
  hardware.bluetooth.enable = true;

  hardware.bumblebee.enable = true;
  hardware.bumblebee.driver = "nvidia";
  # TODO see this
  # hardware.nvidia.modesetting.enable
  # TODO for hardware acceleration
  # hardware.opengl.extraPackages

  # TODO see if enable cpu microcode update

  # TODO add path to config (default.pa)
  hardware.pulseaudio.enable = true;

  # replace same content with hardlink
  # nix.autoOptimiseStore = true;
  # TODO see auto gc for nix

  # allow suspend or RAM
  powerManagement.enable = true;
  # TODO use this to call screen lock after weak up
  # powerManagement.resumeCommands

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.fedeizzo = {
    name = "fedeizzo";
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "input" "video" "bumblebee" "docker" "autologin" "informant" "users"];
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

  #programs = {
  #  qt5ct.enable = true;
  #  zsh = {
  #    enable = true;
  #    enableCompletion = true;
  #    #enableBashCompletion = true;
  #    autosuggestions.enable = true;
  #    histFile = "$HOME/.local/share/history";
  #    syntaxHighlighting.enable = true;
  #  };
  #  bash = {
  #    enableCompletion = true;
  #    enableLsColors = true;
  #  };
  #  thefuck.enable = true;
  #  ccache.enable = true;
  #  file-roller.enable = true;
  #  firejail.enable = true;
  #  less.enable = true;
  #  light.enable = true;
  #};


  virtualisation.docker = {
    enable = true;
    # enableNvidia = true;
    enableOnBoot = true;
  }
}

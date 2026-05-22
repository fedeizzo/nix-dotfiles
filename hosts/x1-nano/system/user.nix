{ username, pkgs, config, inputs, ... }:

{
  imports = [
    inputs.dms.nixosModules.greeter
  ];
  users.mutableUsers = false;
  programs.fuse.userAllowOther = true;
  users.users = {
    ${username} = {
      name = username;
      isNormalUser = true;
      createHome = true;
      extraGroups = [
        "wheel"
        "input"
        "uinput"
        "video"
        "bumblebee"
        "docker"
        "users"
        "networkmanager"
        "libvirtd"
        "audio"
        "dialout" # used to allow flash over serial port without root user
        "adbusers" # for adb android
        "keys" # required to have read access to /run/secrets.d (sops-nix)
        "greeter"
      ];
      shell = pkgs.fish;
      hashedPasswordFile = config.sops.secrets."${username}-path".path;
    };
    root = {
      hashedPassword = "!";
    };
  };

  # services = {
  #   xserver.enable = true;
  #   displayManager.sddm.enable = true;
  #   desktopManager.plasma6.enable = true;
  #   displayManager.sddm.wayland.enable = true;
  # };
  environment.plasma6.excludePackages = with pkgs.kdePackages; [
    plasma-browser-integration
    konsole
    ark
    elisa
    gwenview
    okular
    kate
    khelpcenter
    print-manager
  ];
  programs.dank-material-shell.greeter = {
    enable = true;
    compositor.name = "niri";
    compositor.customConfig = ''
      hotkey-overlay {
        skip-at-startup
      }
      environment {
        DMS_RUN_GREETER "1"
      }
      gestures {
        hot-corners {
          off
        }
      }
      layout {
        background-color "#000000"
      }
    '';
    logs = {
      save = true;
      path = "/tmp/dms-greeter.log";
    };
  };
  programs.dconf.enable = true;
  environment.pathsToLink = [ "/share/applications" "/share/xdg-desktop-portal" ];
  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;
  };
}

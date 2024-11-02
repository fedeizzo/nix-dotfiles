{ pkgs, ... }:

{
  users.users = {
    root = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILj7IsDH+Zjvb8wx22OkYxFtS6u4CssIkFQ3S8xtCVkz federico@fedeizzo.dev"
      ];
      password = "root";
    };
    fridge = {
      isNormalUser = true;
      createHome = true;
      extraGroups = [
        "wheel"
        "input"
        "video"
        "bumblebee"
        "docker"
        "users"
        "networkmanager"
        "audio"
        "dialout" # used to allow flash over serial port without root user
        "adbusers" # for adb android
        "keys" # required to have read access to /run/secrets.d (sops-nix)
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    bc
    curl
    killall
    wget
    git
    vim
    dnsmasq
    hostapd
    htop
  ];

  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
  };

  environment = {
    shellAliases = {
      "update_docker_images" = ''docker images --format "{{.Repository}}:{{.Tag}}" | xargs -L1 docker pull'';
      "restart_docker_containers" = "systemctl restart docker-*.service";
      "SS" = "systemctl";
    };
  };
}

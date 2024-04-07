{ pkgs, ... }:

{
  users.users = {
    root = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJAkXdVG1SxbY+xRXCR66xEnscIWdvzrp1soLQ2SnCUX federico@fedeizzo.dev"
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    raspberrypifw
    bc
    curl
    killall
    wget
    git
    vim
    # docker-compose
    dnsmasq
    hostapd
    raspberrypi-eeprom
    libraspberrypi
    libgpiod
    borgbackup
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
    };
  };
}

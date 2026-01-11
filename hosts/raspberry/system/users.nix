{ pkgs, ... }:

{
  users.users = {
    root = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILj7IsDH+Zjvb8wx22OkYxFtS6u4CssIkFQ3S8xtCVkz federico@fedeizzo.dev"
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
    dnsmasq
    hostapd
    raspberrypi-eeprom
    libraspberrypi
    libgpiod
    htop
  ];

  programs.bash = {
    completion.enable = true;
    enableLsColors = true;
  };
}

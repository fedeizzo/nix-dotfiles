{ config, pkgs, ... }:

{
  users.users = {
    root = {
      openssh.authorizedKeys.keyFiles = [
        config.sops.secrets.laptop-ssh-public-key.path
        config.sops.secrets.deploy-homelab-ssh-public-key.path
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    tailscale
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
    firefox
    raspberrypi-eeprom
    libraspberrypi
    libgpiod
    borgbackup
    kubectl
    kubernetes-helm
    htop
  ];

  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
  };

  environment = {
    shellAliases = {
      "k3sapply" = "find /etc/homelab-kubernetes -type l -name '*apply*' | sort | xargs -I sub k3s kubectl apply -f sub";
      "k3sdelete" = "find /etc/homelab-kubernetes -type l -name '*delete*' | sort -r | xargs -I sub k3s kubectl delete -f sub";
      "update_docker_images" = ''docker images --format "{{.Repository}}:{{.Tag}}" | xargs -L1 docker pull'';
      "restart_docker_containers" = "systemctl restart docker-*.service";
    };
    shellInit = ''
      export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
    '';
  };
}

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
    bc
    curl
    killall
    wget
    git
    vim
    dnsmasq
    hostapd
    htop
    bottom

    # framework desktop specific
    amdgpu_top
    tpm2-tss
    # rocmPackages.amdsmi
    rocmPackages.rocm-smi
    rocmPackages.rocminfo
    btop-rocm
  ];

  programs.bash = {
    completion.enable = true;
    enableLsColors = true;
  };

  environment = {
    shellAliases = {
      "update_docker_images" = ''docker images --format "{{.Repository}}:{{.Tag}}" | xargs -L1 docker pull'';
      "restart_docker_containers" = "systemctl restart docker-*.service";
      "SS" = "systemctl";
      "highlight" = "grep --color -z";
    };
  };
}

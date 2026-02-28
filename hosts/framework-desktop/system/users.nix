{ pkgs, ... }:

{
  users.users = {
    root = {
      password = "test";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILj7IsDH+Zjvb8wx22OkYxFtS6u4CssIkFQ3S8xtCVkz federico@fedeizzo.dev"
        # TODO persist
        # -rw------- 1 root root  399 Feb 28 16:15 ssh_host_ed25519_key
        # -rw-r--r-- 1 root root   94 Feb 28 16:15 ssh_host_ed25519_key.pub
        # -rw------- 1 root root 3381 Feb 28 16:15 ssh_host_rsa_key
        # -rw-r--r-- 1 root root  738 Feb 28 16:15 ssh_host_rsa_key.pub
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

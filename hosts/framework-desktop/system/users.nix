{ pkgs, ... }:

{
  users.users = {
    root = {
      password = "test";
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
    fzf
    bat
    fd
    eza
    bottom

    # framework desktop specific
    amdgpu_top
    tpm2-tss
    # rocmPackages.amdsmi
    rocmPackages.rocm-smi
    rocmPackages.rocminfo
    btop-rocm
  ];
  programs = {
    bash = {
      completion.enable = true;
      enableLsColors = true;
      shellAliases = {
        ls = "eza --icons --sort=type";
        ll = "eza -l --icons --sort=type";
        lll = "eza -l --icons --sort=type | less";
        lla = "eza -la --icons --sort=type";
        llt = "eza -T --icons --sort=type";
        cat = "bat";
        find = "fd";
      };
    };
    starship = {
      enable = true;
      interactiveOnly = true;
      settings = {
        add_newline = false;
        character = {
          success_symbol = "[λ](green)";
          vicmd_symbol = "[V](green)";
          error_symbol = "[✖](red)";
        };
        package.disabled = true;
        directory.truncation_length = 8;
        cmd_duration = {
          min_time = 20000;
          format = "  [$duration](bold yellow)";
        };
      };
    };
    fzf.fuzzyCompletion = true;
    fzf.keybindings = true;
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

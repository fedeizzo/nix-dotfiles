# Headless server baseline: root access, hardened sshd, CLI tools, nix housekeeping.
{
  flake.modules.nixos.server = { pkgs, ... }: {
    documentation.nixos.enable = false;

    nix = {
      gc = {
        automatic = true;
        dates = "daily";
        options = "--delete-older-than 10d";
      };
      optimise.automatic = true;
      extraOptions = ''
        min-free = ${toString (100 * 1024 * 1024)}
        max-free = ${toString (1024 * 1024 * 1024)}
      '';
    };

    users.users.root = {
      hashedPassword = "$y$j9T$tH3Iu/T7QJDOwQY4H/.vR1$Ub3s.9LSZrJ8BBZU1Rn00pbZufmwO2mit4LBVCCbf7A";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILj7IsDH+Zjvb8wx22OkYxFtS6u4CssIkFQ3S8xtCVkz federico@fedeizzo.dev"
      ];
    };

    services.openssh = {
      enable = true;
      allowSFTP = false; # Don't set this if you need sftp
      openFirewall = false;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        X11Forwarding = false;
        PermitRootLogin = "yes";
      };
    };

    environment.systemPackages = with pkgs; [
      # Ghostty's ssh-terminfo installs into ~/.terminfo, which the ephemeral root wipes.
      ghostty.terminfo
      htop
      fzf
      bat
      fd
      eza
      bottom
    ];

    programs = {
      bash = {
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

      fzf.fuzzyCompletion = true;
      fzf.keybindings = true;
    };

    environment.shellAliases.highlight = "grep --color -z";
  };
}

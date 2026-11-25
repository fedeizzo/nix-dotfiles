{
  flake.modules.darwin.macbook = { self, inputs, username, pkgs, ... }: {
    imports = with self.modules.darwin; [
      nix-core
      fonts
      home-manager
      ai-tools
      brew
      vicinae
    ];

    programs.zsh = {
      enable = true;
      # Home Manager / oh-my-zsh already run compinit and set the prompt;
      # skip the duplicate work in /etc/zshrc.
      enableGlobalCompInit = false;
      promptInit = "";
    };
    users.users.${username} = {
      home = "/Users/${username}";
      shell = pkgs.zsh;
    };

    system.primaryUser = username;
    security.pam.services.sudo_local = {
      enable = true;
      reattach = true;
      touchIdAuth = true;
    };
    home-manager.users.${username} = {
      home = {
        inherit username;
        stateVersion = "25.05";
        homeDirectory = "/Users/${username}";
      };

      imports = with self.modules.homeManager; [
        languages
        direnv
        # macOS host: all cli domains except cli-desktop (Linux-only anyway).
        cli-core
        cli-net
        cli-vcs
        cli-secrets
        cli-docs
        cli-media
        cli-nix
        jujutsu
        zsh
        starship
        git
        zen
        zed
        ghostty
        # claude
        # antigravity
        profile-work
        macbook-jj-signing
        dogbrew
        ai-tools
        nono
        herdr-app
      ];

      home.packages = with pkgs; [ pngpaste ];
    };
  };
}

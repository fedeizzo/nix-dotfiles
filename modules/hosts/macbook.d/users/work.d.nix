{
  flake.modules.darwin.macbook = { self, inputs, username, pkgs, ... }: {
    imports = with self.modules.darwin; [
      nix-core
      user-work
      fonts
      home-manager
      nh
    ];

    programs.zsh.enable = true;
    users.users.${username} = {
      home = "/Users/${username}";
      shell = pkgs.zsh;
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
        cli-packages
        jujutsu
        zsh
        starship
        git
        zen
        zed
        profile-work
      ];

      home.packages = with pkgs; [ pngpaste ];
    };
  };
}

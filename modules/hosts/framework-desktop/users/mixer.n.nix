{
  flake.modules.nixos.framework-desktop = { pkgs, inputs, pkgs-unstable, ... }: {
    imports = [
      inputs.self.modules.nixos.ai-tools
    ];

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {
        inherit inputs;
        inherit pkgs-unstable;
        username = "mixer";
      };
      backupFileExtension = "to_delete";

      users.mixer = {
        imports = with inputs.self.modules.homeManager; [
          aichat
          # nono
          fence
          antigravity
          cli-packages
          direnv
          jujutsu
          zsh
          git
          starship
          languages
          nix-index
          profile-personal
          jail-pi
          herdr
        ];
        home = {
          stateVersion = "25.05";
          homeDirectory = "/home/mixer";
          username = "mixer";
        };
        programs.home-manager.enable = true;

        nixpkgs.config = {
          allowUnfree = true;
          permittedInsecurePackages = [ ];
        };
        xdg.configFile."nixpkgs/config.nix".text = ''
          {
            allowUnfree = true;
            permittedInsecurePackages = [ ];
          }
        '';
      };
    };

    users.users = {
      mixer = {
        name = "mixer";
        isNormalUser = true;
        createHome = true;
        description = "Mixer";
        extraGroups = [ "networkmanager" "wheel" "docker" ];
        shell = pkgs.zsh;
        hashedPassword = "$y$j9T$tH3Iu/T7QJDOwQY4H/.vR1$Ub3s.9LSZrJ8BBZU1Rn00pbZufmwO2mit4LBVCCbf7A";
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILj7IsDH+Zjvb8wx22OkYxFtS6u4CssIkFQ3S8xtCVkz federico@fedeizzo.dev"
        ];
      };
    };
  };
}

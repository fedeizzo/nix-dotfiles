# The `anvil` login on forge, with its home-manager environment.
{
  flake.modules.nixos.forge = { pkgs, inputs, pkgs-unstable, username, ... }: {
    imports = [
      inputs.home-manager.nixosModules.home-manager
    ];

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {
        inherit inputs;
        inherit pkgs-unstable;
        inherit username;
      };
      backupFileExtension = "to_delete";

      users.${username} = {
        imports = with inputs.self.modules.homeManager; [
          antigravity
          codex
          claude
          cli-packages
          direnv
          jujutsu
          zsh
          git
          starship
          languages
          nix-index
          profile-personal
          pi
          herdr
          ssh-agent
        ];
        home = {
          stateVersion = "26.05";
          homeDirectory = "/home/${username}";
          username = username;
        };
        programs.home-manager.enable = true;

        xdg.configFile."nixpkgs/config.nix".text = ''
          {
            allowUnfree = true;
            permittedInsecurePackages = [ ];
          }
        '';
      };
    };

    users.users = {
      ${username} = {
        name = username;
        isNormalUser = true;
        createHome = true;
        description = "Anvil";
        extraGroups = [ "networkmanager" "wheel" "docker" "render" "video" ];
        shell = pkgs.zsh;
        hashedPassword = "$y$j9T$tH3Iu/T7QJDOwQY4H/.vR1$Ub3s.9LSZrJ8BBZU1Rn00pbZufmwO2mit4LBVCCbf7A";
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILj7IsDH+Zjvb8wx22OkYxFtS6u4CssIkFQ3S8xtCVkz federico@fedeizzo.dev"
        ];
      };
    };

    # Root is wiped on boot (see ephemeral-root); keep the home directory.
    environment.persistence."/persist".directories = [
      {
        directory = "/home/${username}";
        user = username;
        group = "users";
        mode = "u=rwx,g=,o=";
      }
    ];
  };
}

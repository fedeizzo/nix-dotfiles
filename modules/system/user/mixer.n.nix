# The `mixer` login on server hosts, with its home-manager environment.
{
  flake.modules.nixos.mixer = { pkgs, inputs, pkgs-unstable, ... }: {
    imports = [
      inputs.home-manager.nixosModules.home-manager
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
          # aichat
          # nono
          # fence
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
          pi
          herdr
          ssh-agent
        ];
        home = {
          stateVersion = "25.05";
          homeDirectory = "/home/mixer";
          username = "mixer";
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
      mixer = {
        name = "mixer";
        isNormalUser = true;
        createHome = true;
        description = "Mixer";
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
        directory = "/home/mixer";
        user = "mixer";
        group = "users";
        mode = "u=rwx,g=,o=";
      }
    ];
  };
}

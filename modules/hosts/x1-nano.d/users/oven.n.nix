{
  flake.modules.nixos.x1-nano = { username, pkgs, inputs, config, pkgs-unstable, ... }: {
    nixpkgs.overlays = [
      inputs.llm-agents.overlays.default
      inputs.niri.overlays.niri
    ];
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {
        inherit (config) sops;
        inherit username;
        inherit inputs;
        inherit pkgs-unstable;
      };
      backupFileExtension = "to_delete";
      sharedModules = [
        inputs.niri.homeModules.niri
        inputs.dms.homeModules.dank-material-shell
        inputs.dms.homeModules.niri
        inputs.dms-plugin-registry.homeModules.default
      ];

      users.${username} = {
        imports = [
          inputs.self.modules.homeManager.bottom
          inputs.self.modules.homeManager.cli-packages
          inputs.self.modules.homeManager.direnv
          inputs.self.modules.homeManager.jujutsu
          inputs.self.modules.homeManager.desktop-environment
          inputs.self.modules.homeManager.zen
          inputs.self.modules.homeManager.fish
          inputs.self.modules.homeManager.git
          inputs.self.modules.homeManager.starship
          inputs.self.modules.homeManager.languages
          inputs.self.modules.homeManager.nix-index
          inputs.self.modules.homeManager.jail-pi
          inputs.self.modules.homeManager.misc
          inputs.self.modules.homeManager.nextcloud
          inputs.self.modules.homeManager.ssh
          inputs.self.modules.homeManager.nix-registry
          inputs.self.modules.homeManager.solaar
          inputs.self.modules.homeManager.stylix
          inputs.self.modules.homeManager.zathura
          inputs.self.modules.homeManager.zed
          inputs.self.modules.homeManager.profile-personal

          inputs.self.modules.homeManager.kitty
        ];
        programs.home-manager.enable = true;

        home = {
          stateVersion = "25.05";
          homeDirectory = "/home/${username}";
          username = "${username}";
        };

        nixpkgs.config = {
          allowUnfree = true;
          joypixels.acceptLicense = true;
          permittedInsecurePackages = [ ];
        };
        xdg.configFile."nixpkgs/config.nix".text = ''
          {
            allowUnfree = true;
            joypixels.acceptLicense = true;
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
        extraGroups = [
          "wheel"
          "input"
          "uinput"
          "video"
          "bumblebee"
          "docker"
          "users"
          "networkmanager"
          "libvirtd"
          "audio"
          "dialout" # used to allow flash over serial port without root user
          "adbusers" # for adb android
          "keys" # required to have read access to /run/secrets.d (sops-nix)
          "greeter"
        ];
        shell = pkgs.fish;
        hashedPasswordFile = config.sops.secrets."${username}-path".path;
      };
      root = {
        hashedPassword = "!";
      };
    };

    sops.secrets."${username}-path" = {
      neededForUsers = true;
    };
  };
}

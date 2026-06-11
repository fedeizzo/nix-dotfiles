{ username
, inputs
, config
, emacs-pkg
, pkgs-unstable
, ...
}:


{
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
      inherit emacs-pkg;
      inherit pkgs-unstable;
    };
    backupFileExtension = "to_delete";
    sharedModules = [
      inputs.plasma-manager.homeModules.plasma-manager
      inputs.niri.homeModules.niri
      inputs.dms.homeModules.dank-material-shell
      inputs.dms.homeModules.niri
      inputs.dms-plugin-registry.modules.default
    ];

    users.${username} = {
      imports = [
        inputs.self.modules.homeManager.bottom
        inputs.self.modules.homeManager.cli-packages
        inputs.self.modules.homeManager.direnv
        inputs.self.modules.homeManager.jujutsu
        ./modules/desktop-environment
        inputs.self.modules.homeManager.zen
        inputs.self.modules.homeManager.fish
        inputs.self.modules.homeManager.git
        inputs.self.modules.homeManager.starship
        inputs.self.modules.homeManager.languages
        inputs.self.modules.homeManager.nix-index
        inputs.self.modules.homeManager.jail-pi
        ./modules/misc
        ./modules/nextcloud
        inputs.self.modules.homeManager.ssh
        inputs.self.modules.homeManager.nix-registry
        ./modules/plasma
        ./modules/solaar
        ./modules/stylix
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

      nixpkgs.config = import ./config.nix;
      xdg.configFile."nixpkgs/config.nix".source = ./config.nix;
    };
  };
}

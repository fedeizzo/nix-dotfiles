{ pkgs, username, inputs, lib, ... }:


{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = { inherit username; };
    users.${username} = {
      imports = [
        inputs.hyprland.homeManagerModules.default
        ./modules/desktop
      ];
      programs.home-manager.enable = true;
      home = {
        stateVersion = "22.11";
        homeDirectory = "/home/${username}";
        username = "${username}";
      };
      nixpkgs.config = import ./config.nix;
      xdg.configFile."nixpkgs/config.nix".source = ./config.nix;
    };
  };
}

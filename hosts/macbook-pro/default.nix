{ inputs, pkgs, username, system-overlays, emacs-pkg, pkgs-unstable, ... }:

{
  imports = [
    inputs.home-manager.darwinModules.home-manager
    # (import ../common/nh { inherit username; isMac = true; })
  ];

  # nix
  system.stateVersion = 4;
  nix = {
    enable = true;
    settings = {
      experimental-features = "nix-command flakes";
    };
    extraOptions = ''
      auto-optimise-store = true
      experimental-features = nix-command flakes
      extra-platforms = x86_64-darwin aarch64-darwin
    '';
    optimise.automatic = true;
  };
  nixpkgs.overlays = builtins.attrValues system-overlays;

  # user
  programs.zsh.enable = true;
  fonts.packages = [ pkgs.nerd-fonts.jetbrains-mono ];
  users.users.${username} = {
    home = "/Users/${username}";
    shell = pkgs.zsh;
  };
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit username inputs emacs-pkg pkgs-unstable;
    };

    users.${username} = import ../../home/macbook-pro;
    backupFileExtension = "backup";
  };
  ids.gids.nixbld = 350;
}

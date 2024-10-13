{ inputs, pkgs, username, system-overlays, ... }:

{
  imports = [
    inputs.home-manager.darwinModules.home-manager
    inputs.nh-darwin.nixDarwinModules.prebuiltin
    (import ../common/nh { inherit username; isMac = true; })
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit username inputs;
    };

    users.${username} = import ../../home/macbook-pro;
  };

  system.stateVersion = 4;

  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;
  nix.settings.experimental-features = "nix-command flakes";
  nix.extraOptions = ''
    auto-optimise-store = true
    experimental-features = nix-command flakes
    extra-platforms = x86_64-darwin aarch64-darwin
  '';
  programs.zsh.enable = true;

  users.users.${username} = {
    home = "/Users/${username}";
    shell = pkgs.zsh;
  };
  nixpkgs.overlays = builtins.attrValues system-overlays;
}

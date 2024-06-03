{ inputs, pkgs, username, ... }:

{
  imports = [
    # ../common/nh
  ];

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

  environment.shellAliases.nh = "nh-darwin";
}

{ pkgs, username, inputs, config, nixpkgs-unstable, lib, ... }:


{
  home = {
    username = "federico.izzo";
    stateVersion = "22.11";
    homeDirectory = "/Users/federico.izzo";
  };
  imports = [
    # (import ../common/rofi
    #   {
    #     pkgs = pkgs;
    #     rofiPackage = pkgs.rofi;
    #   }
    # )
    ../common/kitty
    ../common/fish
    ../common/emacs
    ./modules/languages.nix
    ./modules/cli/default.nix
  ];
  fishPerMachine.aliases = {
    ciao = "echo 'hello world'";
  };
  home.packages = with pkgs; [ pngpaste ];
programs.zsh.initExtra = "if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi";
}

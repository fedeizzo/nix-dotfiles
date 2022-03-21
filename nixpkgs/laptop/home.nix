{ pkgs, inputs, system, ... }:

let
  neovim-nightly = inputs.neovim-nightly-overlay.packages.${system}.neovim;
in
{
  imports = [
    ../modules/bottom.nix
    ../modules/chat.nix
    ../modules/cli.nix
    ../modules/config.nix
    ../modules/emacs.nix
    ../modules/fish.nix
    ../modules/git.nix
    ../modules/kitty.nix
    ../modules/languages.nix
    ../modules/lf.nix
    ../modules/media.nix
    ../modules/neovim.nix
    ../modules/notes.nix
    ../modules/packages.nix
    ../modules/qutebrowser.nix
    ../modules/services.nix
    ../modules/wayland.nix
    ../modules/zathura.nix
  ];

  home.stateVersion = "21.11";
  home.homeDirectory = "/home/fedeizzo";
  home.username = "fedeizzo";
  nixpkgs.config = import ./config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./config.nix;



  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "org.pwmt.zathura.desktop";
      "inode/directory" = "lf.desktop";
      "text/plain" = "nvim.desktop";
      "text/html" = "org.qutebrowser.qutebrowser.desktop";
      "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
      "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
    };
  };
}

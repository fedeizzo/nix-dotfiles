{ pkgs, inputs, system, ... }:

let
  neovim-nightly = inputs.neovim-nightly-overlay.packages.${system}.neovim;
in
{
  imports = [
    ../modules/chat.nix
    ../modules/config.nix
    ../modules/cuda.nix
    ../modules/fish.nix
    ../modules/languages.nix
    ../modules/media.nix
    ../modules/neovim.nix 
    ../modules/packages.nix
    ../modules/services.nix
    ../modules/st.nix
    ../modules/xmonad.nix
  ];

  home.stateVersion = "20.09";
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

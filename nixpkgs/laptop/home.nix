{ pkgs, ... }:
let
  sources = import ../nix/sources.nix;
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

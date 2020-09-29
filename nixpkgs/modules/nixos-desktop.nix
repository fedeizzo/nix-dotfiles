{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Meslo" "FiraCode" ]; })
    arandr
    bitwarden
    bitwarden-cli
    firefox
    haskellPackages.greenclip
    libreoffice
    openssh
    pandoc
    qutebrowser
    rofi
    spotify
    # simplescreenrecorder
    # steam
  ];

  # ROFI
  xdg.configFile."rofi/config.rasi".source = ../../dotfiles/dot_config/rofi/config.rasi;

  # QUTEBROWSER
  home.file.".config/qutebrowser" = {
    source = ../../dotfiles/dot_config/private_qutebrowser;
    recursive = true;
  };
  home.file.".local/share/qutebrowser/userscripts/rofiQutebrowser" = {
    source = ../../dotfiles/dot_local/share/private_qutebrowser/userscripts/executable_rofiQutebrowser;
    executable = true;
  };
  home.file.".config/qutebrowser/browser-home" = {
    source = ../../browser-home;
    recursive = true;
  };

  # GREENCLIP
  xdg.configFile."greenclip.cfg".source = ../../dotfiles/dot_config/greenclip.cfg;
}


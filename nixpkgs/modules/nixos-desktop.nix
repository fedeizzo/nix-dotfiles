{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    (nerdfonts.override {
      fonts = [
        "3270"
        "Agave"
        "AnonymousPro"
        "Arimo"
        "BitstreamVeraSansMono"
        "CascadiaCode"
        "Cousine"
        "DaddyTimeMono"
        "DejaVuSansMono"
        "FantasqueSansMono"
        "FiraCode"
        "Go-Mono"
        "Gohu"
        "Hack"
        "HeavyData"
        "IBMPlexMono"
        "Inconsolata"
        "InconsolataGo"
        "InconsolataLGC"
        "Iosevka"
        "JetBrainsMono"
        "Lekton"
        "LiberationMono"
        "MPlus"
        "Meslo"
        "Monofur"
        "Monoid"
        "Mononoki"
        "Noto"
        "ProFont"
        "ProggyClean"
        "RobotoMono"
        "ShareTechMono"
        "SourceCodePro"
        "SpaceMono"
        "Terminus"
        "Tinos"
        "Ubuntu"
        "UbuntuMono"
        "VictorMono"
        "iA-Writer"
      ];
    })
    arandr
    autorandr
    bitwarden
    bitwarden-cli
    brightnessctl
    bspwmbar
    firefox
    # haskellPackages.greenclip
    xcmenu
    jq
    keyutils
    libreoffice
    mpd-mpris
    multilockscreen
    openssh
    pamixer
    pandoc
    pavucontrol
    playerctl
    qutebrowser
    # rofi
    xorg.xmodmap
    devour
    xss-lock
    # simplescreenrecorder
    # steam
  ];

  # ROFI
  # xdg.configFile."rofi/config.rasi".source = ../../dotfiles/dot_config/rofi/config.rasi;

  # QUTEBROWSER
  home.file = {
    ".config/qutebrowser/config.py" = {
      source = ../../dotfiles/dot_config/private_qutebrowser/config.py;
    };
    ".config/qutebrowser/nord-qutebrowser.py" = {
      source = ../../dotfiles/dot_config/private_qutebrowser/nord-qutebrowser.py;
    };
    ".config/qutebrowser/qsettings/QtProject.conf" = {
      source = ../../dotfiles/dot_config/private_qutebrowser/qsettings/QtProject.conf;
    };
    # ".local/share/qutebrowser/userscripts/rofiQutebrowser" = {
    #   source = ../../dotfiles/dot_local/share/private_qutebrowser/userscripts/executable_rofiQutebrowser;
    #   executable = true;
    # };
    ".config/qutebrowser/browser-home" = {
      source = ../../browser-home;
      recursive = true;
    };
  };
  # GREENCLIP
  # xdg.configFile."greenclip.cfg".source = ../../dotfiles/dot_config/greenclip.cfg;

  # XMONAD
  home.file.".xprofile" = {
    source = ../../dotfiles/dot_xmonad/xmonad-session-rc;
    executable = true;
  };
  home.file.".xmonad/xmonad.hs".source = ../../dotfiles/dot_xmonad/xmonad.hs;
  home.file.".xmonad/xmobar.hs".source = ../../dotfiles/dot_xmonad/xmobar.hs;
  home.file.".xmonad/bin/trayer-padding-icon" = {
    source = ../../dotfiles/dot_xmonad/bin/trayer-padding-icon;
    executable = true;
  };

  # BSPWM
  xdg.configFile."bspwm/pacwall.png".source = ../../dotfiles/dot_config/bspwm/pacwall.png;
  home.file."./.config/bspwm/bspwmrc" = {
    source = ../../dotfiles/dot_config/bspwm/executable_bspwmrc;
    executable = true;
  };
  # SXHKD
  xdg.configFile."sxhkd/sxhkdrc".source = ../../dotfiles/dot_config/sxhkd/sxhkdrc;

  # BETTERLOCKSCREEN
  # xdg.configFile."betterlockscreenrc".source = ../../dotfiles/dot_config/betterlockscreenrc;

  # DUNST
  xdg.configFile."dunst/dunstrc".source = ../../dotfiles/dot_config/dunst/dunstrc;
  xdg.configFile."dunst/critical.png".source = ../../dotfiles/dot_config/dunst/critical.png;
  xdg.configFile."dunst/normal.png".source = ../../dotfiles/dot_config/dunst/normal.png;
  xdg.configFile."dunst/low.png".source = ../../dotfiles/dot_config/dunst/low.png;

  # LF
  xdg.configFile."lf/lfrc".source = ../../dotfiles/dot_config/lf/lfrc;

  # NEOFETCH
  xdg.configFile."noefetch/config.conf".source = ../../dotfiles/dot_config/neofetch/config.conf;

  # PICOM 
  xdg.configFile."picom/picom.conf".source = ../../dotfiles/dot_config/picom/picom.conf;

  # STARSHIP
  xdg.configFile."starship.toml".source = ../../dotfiles/dot_config/starship.toml;

  # Xresources
  home.file.".Xresources" = {
    source = ../../dotfiles/dot_Xresources;
  };

  # xinitrc
  home.file.".xinitrc" = {
    source = ../../dotfiles/executable_dot_xinitrc;
    executable = true;
  };

  # PERSONAL SCRIPTS
  home.file.".sources" = {
    source = ../../sources;
    executable = true;
    recursive = true;
  };
}

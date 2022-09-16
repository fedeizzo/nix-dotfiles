{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    keyutils
    qgis
    universal-ctags
    onlyoffice-bin
    # fonts
    joypixels
    (nerdfonts.override {
      fonts = [
        "Meslo"
        "RobotoMono"
      ];
    })
    # audio
    pavucontrol
    # browser
    firefox
    google-chrome
    # media
    mpv
    streamlink
    vlc
    spotify
    gimp
    calibre
    # notes
    zotero
    anki
    inkscape-with-extensions
    # hugo
    # (unstable.xournalpp.overrideAttrs (old: rec {
    #   vi-xournalpp = fetchFromGitHub {
    #     owner = "raw-bacon";
    #     repo = "vi-xournalpp";
    #     rev = "master";
    #     sha256 = "sha256-IjJTVW9xbsMz6C78HE/uFahcA4sXxr6nx6r5lqK4O7Y=";
    #   };
    #   doDist = true;
    #   distPhase = ''
    #     mkdir -p $out/share/xournalpp/plugins/vi-xournalpp
    #     cp ${vi-xournalpp.out}/* $out/share/xournalpp/plugins/vi-xournalpp
    #   '';
    # }))
    drawio
    pdf2svg
    yubikey-manager
    yubikey-manager-qt
    networkmanagerapplet
  ];
}

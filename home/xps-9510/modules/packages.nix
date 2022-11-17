{ pkgs, nix-bubblewrap, pkgs-unstable, ... }:

let
  inherit (nix-bubblewrap.lib.x86_64-linux) wrapPackage;
in
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
    # (wrapPackage {
    #   package = pkgs.firefox;
    #   options = [
    #     "-gpu"
    #     "-net"
    #     "-pulse"
    #   ];
    # })
    # spellchecking
    languagetool
    pkgs-unstable.bitwarden
    pkgs-unstable.tdesktop
    pkgs-unstable.xournalpp
  ];
}

{ pkgs, nix-bubblewrap, pkgs-unstable, ... }:

let
  inherit (nix-bubblewrap.lib.x86_64-linux) wrapPackage;
in
{
  home.packages = with pkgs; [
    keyutils
    universal-ctags
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
    # google-chrome
    # media
    mpv
    streamlink
    vlc
    spotify
    gimp
    pkgs-unstable.calibre
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
    element-desktop-wayland
    (makeDesktopItem {
      name = "Element";
      exec = "element-desktop";
      comment = "Element matrix client";
      desktopName = "Element";
      type = "Application";
      mimeTypes = [ ];
    })
    gnome-solanum
    libreoffice
  ];
  programs.garminDB = {
    enable = true;
    config = {
      credentials = {
        user = "federico.izzo99@gmail.com";
        secure_password = true;
        password_command = "rbw get garmin";
      };
      data = {
        weight_start_date = "12/20/2022";
        sleep_start_date = "12/20/2022";
        rhr_start_date = "12/20/2022";
        monitoring_start_date = "12/20/2022";
        download_latest_activities = 25;
        download_all_activities = 100;
      };
      copy.mount_dir = "/volumes/garmin";
      enabled_stats = {
        monitoring = true;
        steps = true;
        itime = true;
        sleep = true;
        rhr = true;
        weight = true;
        activities = true;
      };
      course_views.steps = [ ];
      modes = { };
      activities.display = [ ];
    };
  };
}

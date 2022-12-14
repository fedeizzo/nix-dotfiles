{ pkgs, username, ... }:

let
  # wobBrightnessPath = "/tmp/wob_brightness.sock";
  # nextWorkspace = pkgs.writeShellScriptBin "next_workspace" ''
  #   current=$(swaymsg -t get_workspaces | ${pkgs.jq}/bin/jq -r '.[] | select(.focused==true) | .name')
  #   swaymsg workspace $(echo "($current + 1)%5" | ${pkgs.bc}/bin/bc)
  # '';
  # prevWorkspace = pkgs.writeShellScriptBin "prev_workspace" ''
  #   current=$(swaymsg -t get_workspaces | ${pkgs.jq}/bin/jq -r '.[] | select(.focused==true) | .name')
  #   if [[ $current == 1 ]]; then
  #      prev=5
  #   else
  #      prev=$(echo "$current - 1" | ${pkgs.bc}/bin/bc)
  #   fi
  #   swaymsg workspace $prev
  # '';
  twitchStreamlink = pkgs.writeShellScriptBin "twitch_streamlink" ''
    CHANNEL=`${pkgs.gnome.zenity}/bin/zenity --entry --text="Enter channel:"`
    streamlink --player=mpv twitch.tv/$CHANNEL best &
    disown
  '';
  startPlayWithMPVServer = pkgs.writeShellScriptBin "start_play_with_mpv" ''
    konsole -e '${pkgs.play-with-mpv}/bin/play-with-mpv'
  '';
in
{
  imports = [
    ../../../common/rofi
  ];
  gtk = {
    enable = true;
    theme.name = "Nordic";
    theme.package = pkgs.nordic;
  };
  programs.mpv = {
    enable = true;
    config = {
      # profile = "gpu-hq";
      save-position-on-quit = true;
    };
  };
  home.packages = with pkgs; [
    chromium
    rnote
    xournalpp
    pavucontrol
    chromium
    streamlink
    dolphin
    libsForQt5.plasma-systemmonitor
    vlc
    tdesktop
    brightnessctl
    qview
    libsForQt5.okular
    twitchStreamlink
    (makeDesktopItem {
      name = "Twitch streamlink";
      exec = "twitch_streamlink";
      comment = "Twitch streamlink";
      desktopName = "twitch-streamlink";
      type = "Application";
      mimeTypes = [ ];
    })
    play-with-mpv
    startPlayWithMPVServer
    (makeDesktopItem {
      name = "Play with mpv SERVER";
      exec = "start_play_with_mpv";
      comment = "Play with mpv SERVER";
      desktopName = "Play with mpv SERVER";
      type = "Application";
      mimeTypes = [ ];
    })
  ];
  xdg.configFile."images/wallpaper.png" = {
    source = ../../../common/images/wallpaper.png;
  };
  xdg.configFile."images/lock-screen.jpg" = {
    source = ../../../common/images/lock-screen.jpg;
  };
}

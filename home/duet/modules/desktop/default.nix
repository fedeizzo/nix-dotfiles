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
  # wayland.windowManager.hyprland = {
  #   enable = true;
  #   xwayland.enable = true;
  #   recommendedEnvironment = true;
  #   extraConfig = builtins.readFile ./hyprland.conf;
  #   systemdIntegration = true;
  # };
  # wayland.windowManager.sway = {
  #   enable = true;
  #   config = {
  #     modifier = "Mod4";
  #     focus.followMouse = "yes";
  #     focus.newWindow = "smart";
  #     fonts = {
  #       names = [ "JetBrains Mono" "FontAwesome5Free" ];
  #       style = "Bold";
  #       size = 11.0;
  #     };
  #     output = {
  #       DSI-1 = {
  #         bg = "~/.config/images/wallpaper.png fill";
  #         mode = "1920x1200@60Hz";
  #         transform = "270";
  #         scale = "1.25";
  #       };
  #     };
  #     input = {
  #       "type:touch" = {
  #         drag = "enabled";
  #         dwt = "enabled";
  #         tap = "enabled";
  #         tap_button_map = "lrm";
  #         map_to_output = "DSI-1";
  #       };
  #       "10182:3632:hid-over-i2c_27C6:0E30_Stylus" = {
  #         events = "enabled";
  #         drag = "enabled";
  #         dwt = "enabled";
  #         tap = "enabled";
  #         tap_button_map = "lrm";
  #         map_to_output = "DSI-1";
  #       };
  #     };
  #     bars = [ ];
  #     startup = [
  #       { command = "${pkgs.lavalauncher}/bin/lavalauncher"; }
  #       { command = "rm -f ${wobBrightnessPath} && mkfifo ${wobBrightnessPath} && tail -f ${wobBrightnessPath} | wob --anchor top"; always = true; }
  #       { command = "systemctl --user import-environment DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP"; }
  #     ];
  #     keybindings = {
  #       "Mod4+1" = "workspace number 1";
  #       "Mod4+2" = "workspace number 2";
  #       "Mod4+Return" = "exec ${pkgs.foot}/bin/foot";
  #       "Mod4+q" = "kill";
  #       "Mod4+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";
  #       "Mod4+Shift+C" = "reload";
  #       "Mod4+Shift+Escape" = "exit";
  #       "F6" = "exec brightnessctl set 1%- | sed -En 's/.*\\(([0-9]+)%\\).*/\\1/p' > ${wobBrightnessPath}";
  #       "F7" = "exec brightnessctl set +1% | sed -En 's/.*\\(([0-9]+)%\\).*/\\1/p' > ${wobBrightnessPath}";
  #       "F8" = "exec pamixer --decrease 5";
  #       "F9" = "exec pamixer --increase 5";
  #     };
  #   };
  # };
  home.packages = with pkgs; [
    chromium
    rnote
    xournalpp
    pavucontrol
    chromium
    streamlink
    mpv
    dolphin
    libsForQt5.plasma-systemmonitor
    # gnome.dconf-editor
    # gnome.nautilus
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
    # swaybg
    # wl-clipboard
    # clipman
    # imv
    # socat
    # lavalauncher
    # wob
    # pamixer
    # nextWorkspace
    # prevWorkspace
  ];
  xdg.configFile."images/wallpaper.png" = {
    source = ../../../common/images/wallpaper.png;
  };
  xdg.configFile."images/lock-screen.jpg" = {
    source = ../../../common/images/lock-screen.jpg;
  };
  # dconf = {
  #   enable = true;
  #   settings = {
  #     "org/gnome/desktop/background" = {
  #       "picture-uri" = "file:///home/${username}/.config/images/wallpaper.png";
  #       "picture-uri-dark" = "file:///home/${username}/.config/images/wallpaper.png";
  #     };
  #     "org/gnome/desktop/interface" = {
  #       "color-scheme" = "prefer-dark";
  #     };
  #     "org/gnome/desktop/wm/keybindings" = {
  #       "close" = [ "<Super>q" ];
  #     };
  #     "org/gnome/settings-daemon/plugins/media-keys" = {
  #       "on-screen-keyboard" = [ "<Super>F5" ];
  #       "custom-keybindings" = [
  #         "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
  #       ];
  #     };
  #     "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
  #       "binding" = "<Super>b";
  #       "command" = "${pkgs.rofi-rbw}/bin/rofi-rbw";
  #       "name" = "PasswordFiller";
  #     };
  #     "sm/puri/phoc" = {
  #       "auto-maximize" = false;
  #     };
  #   };
  # };
  # programs.eww = {
  #   enable = false;
  #   configDir = ../dotfiles/eww;
  # };
  # xdg.configFile."lavalauncher/images" = {
  #   source = ./lavalauncher_images;
  #   recursive = true;
  # };
  # xdg.configFile."lavalauncher/lavalauncher.conf" = {
  #   source = ./lavalauncher.conf;
  # };
  # xdg.configFile."environment.d/envvars.conf" = {
  #   text = ''
  #     XDG_RUNTIME_DIR=/run/user/1000
  #     WAYLAND_DISPLAY=wayland-1
  #     SDL_VIDEODRIVER=wayland
  #     _JAVA_AWT_WM_NONREPARENTING=1
  #     QT_QPA_PLATFORM=wayland
  #     XDG_SESSION_DESKTOP=hyrpland
  #     MOZ_ENABLE_WAYLAND="1"
  #     XDG_CURRENT_DESKTOP=Unity
  #     XDG_SESSION_TYPE="wayland"
  #   '';
  #   # GTK_USE_PORTAL=0
  # };
  # To check the current gnome configuration you can run
  # `dconf dump / > dconf.settings` and look at the generated
  # file
}

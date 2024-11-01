{ pkgs, ... }:

let
  ddc-audio-up = pkgs.writers.writeBashBin "ddc-audio-up" ''
    ddcutil -d 1 setvcp 62 + 10
    current_audio=$(ddcutil -d 1 getvcp 62 -t | awk -F' ' '{print $4}')
    swayosd-client --custom-message="$current_audio" --custom-icon=multimedia-volume-control
  '';
  ddc-audio-down = pkgs.writers.writeBashBin "ddc-audio-down" ''
    ddcutil -d 1 setvcp 62 - 10
    current_audio=$(ddcutil -d 1 getvcp 62 -t | awk -F' ' '{print $4}')
    swayosd-client --custom-message="$current_audio" --custom-icon=multimedia-volume-control
  '';
in
{
  programs.waybar = {
    enable = true;
    settings = [{
      height = 20;
      spacing = 4;
      layer = "top";
      position = "bottom";
      modules-left = [
        # "hyprland/window"
        "hyprland/workspaces"
        "group/info"
      ];
      modules-center = [ ];
      modules-right = [
        "group/control-left"
        "clock"
        "battery"
        "idle_inhibitor"
        "tray"
      ];

      # modules-left
      "hyprland/workspaces" = {
        format = "{icon}";
        on-click = "activate";
        all-outputs = true;
        format-icons = {
          "1" = "";
          "2" = "󰖟";
          "3" = "󰅩";
          "4" = "󰞶";
          "5" = "";
          "6" = "";
          "7" = "";
        };
      };
      "group/info" = {
        orientation = "inherit";
        drawer = {
          transition-duration = 300;
          transition-left-to-right = false;
        };
        modules = [
          "custom/arrow-right"
          "cpu"
          "memory"
          "disk"
        ];
      };
      "custom/arrow-right" = {
        format = "󰁙";
        tooltip = false;
      };
      cpu = {
        format = "󰘚 {usage}󱉸";
      };
      memory = {
        format = " {:2}󱉸";
      };
      disk = {
        interval = 600;
        format = "󰆼 {percentage_used}󱉸";
        path = "/";
      };

      # modules right
      "group/control-left" = {
        orientation = "inherit";
        modules = [
          "custom/external-monitor-audio"
          "pulseaudio"
          "group/network"
        ];
      };
      pulseaudio = {
        format = "{icon}";
        format-bluetooth = "{icon}";
        tooltip = true;
        format-muted = "󰖁";
        format-icons = {
          headphones = "󰋌";
          handsfree = "󰋌";
          headset = "󰋌";
          phone = "";
          portable = "";
          car = " ";
          default = [
            "󰕿"
            "󰖀"
            "󰕾"
          ];
        };
        on-click = "swayosd-client --output-volume mute-toggle";
        on-click-middle = "pavucontrol";
        on-scroll-up = "swayosd-client --output-volume 5";
        on-scroll-down = "swayosd-client --output-volume -5";
        smooth-scrolling-threshold = 1;
      };
      "group/network" = {
        orientation = "inherit";
        drawer = {
          transition-duration = 300;
          transition-left-to-right = true;
        };
        modules = [
          "network"
          "network#speed"
        ];
      };
      "custom/external-monitor-audio" = {
        exec = "ddcutil -d 1 getvcp 62 -t | awk -F' ' '{print $4}'";
        exec-if = "sleep 1"; ## Give enough time for `sway output` command changes to propagate so we can read them in the next `exec`
        tooltip = true;
        tooltip-format = "{}";
        format = "{icon}";
        format-icons = [ "" ];
        interval = "once";
        on-click = "${ddc-audio-up}/bin/ddc-audio-up";
        on-click-right = "${ddc-audio-down}/bin/ddc-audio-down";
      };
      network = {
        format = "{icon}";
        format-icons = {
          wifi = [
            "󰤨"
          ];
          ethernet = [
            "󰈁"
          ];
          disconnected = [
            ""
          ];
        };
        format-wifi = "󰤨";
        format-ethernet = "󰈁";
        format-disconnected = "󰖪";
        format-linked = "󰈁";
        tooltip = false;
      };
      "network#speed" = {
        format = " {bandwidthDownBits} ";
        interval = 5;
        tooltip-format = "{ipaddr}";
        tooltip-format-wifi = "{essid} ({signalStrength}%)   \n{ipaddr} | {frequency} MHz{icon} ";
        tooltip-format-ethernet = "{ifname} 󰈀 \n{ipaddr} | {frequency} MHz{icon} ";
        tooltip-format-disconnected = "Not Connected to any type of Network";
        tooltip = true;
      };
      clock = {
        format = "{:%H:%M}";
        format-alt = "{:%A %d.%m}";
        tooltip-format = "<tt><big>{calendar}</big></tt>";
        calendar = {
          mode = "month";
          on-scroll = 1;
          format = {
            months = "<span color='#ffead3'><b>{}</b></span>";
            days = "<span color='#ecc6d9'><b>{}</b></span>";
            weeks = "<span color='#99ffdd'><b>W{}</b></span>";
            weekdays = "<span color='#ffcc66'><b>{}</b></span>";
            today = "<span color='#ff6699'><b><u>{}</u></b></span>";
          };
        };
        actions = {
          on-click-right = "mode";
          on-scroll-up = "shift_up";
          on-scroll-down = "shift_down";
        };
      };
      battery = {
        states = {
          good = 95;
          warning = 30;
          critical = 15;
        };
        format = "{icon}";
        format-charging = "<b>{icon} </b>";
        format-full = "<span color='#82A55F'><b>{icon}</b></span>";
        format-icons = [
          "󰁻"
          "󰁼"
          "󰁾"
          "󰂀"
          "󰂂"
          "󰁹"
        ];
        tooltip-format = "{timeTo} {capacity} % | {power} W";
      };
      idle_inhibitor = {
        format = "{icon} ";
        format-icons = {
          activated = "";
          deactivated = "";
        };
      };
      tray = {
        spacing = 10;
      };
    }];
  };
}

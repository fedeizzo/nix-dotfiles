{ config, pkgs, libs, ... }:

{
  programs.autorandr = {
    enable = true;
    profiles = {
      # "hdmi".config = {
      #   DP1.enable = false;
      #   VIRTUAL1.enable = false;
      #   eDP1.enable = false;
      #   HDMI1 = {
      #     enable = true;
      #     crtc = 0;
      #     position = "0x0";
      #     mode = "1920x1080";
      #     rate = "60.00";
      #     primary = true;
      #   };
      # };
      # "laptop".config = {
      #   DP1.enable = false;
      #   VIRTUAL1.enable = false;
      #   eDP1 = {
      #     enable = true;
      #     crtc = 0;
      #     position = "0x0";
      #     mode = "1920x1080";
      #     rate = "60.01";
      #     primary = true;
      #   };
      #   HDMI1.enable = false;
      # };
      # "hdmiUp".config = {
      #   DP1.enable = false;
      #   VIRTUAL1.enable = false;
      #   eDP1 = {
      #     enable = true;
      #     crtc = 1;
      #     position = "0x1080";
      #     mode = "1920x1080";
      #     rate = "60.01";
      #     primary = false;
      #   };
      #   HDMI1 = {
      #     enable = true;
      #     crtc = 0;
      #     position = "0x0";
      #     mode = "1920x1080";
      #     rate = "60.00";
      #     primary = true;
      #   };
      # };
      "home".config = {
        eDP-1 = {
          enable = true;
          crtc = 1;
          position = "0x0";
          mode = "1920x1200";
          rate = "60.01";
          primary = false;
        };
        DP-1.enable = false;
        DP-2.enable = false;
        DP-3 = {
          enable = true;
          crtc = 0;
          position = "1920x0";
          mode = "1920x1080";
          rate = "60.00";
          primary = true;
        };
      };
      # "work".config = {
      #   DP1 = {
      #     enable = true;
      #     crtc = 0;
      #     position = "1920x0";
      #     mode = "1920x1080";
      #     rate = "60.00";
      #     primary = true;
      #   };
      #   VIRTUAL1.enable = false;
      #   eDP1 = {
      #     enable = true;
      #     crtc = 1;
      #     position = "0x0";
      #     mode = "1920x1080";
      #     rate = "60.01";
      #     primary = false;
      #   };
      #   HDMI1.enable = false;
      # };
      "home".fingerprint = {
        DP-3 = builtins.readFile ./DP-3-edid;
        eDP-1 = builtins.readFile ./eDP-1-edid;
      };
      # "hdmi".fingerprint = {
      #   HDMI1 = builtins.readFile ./hdmi1-edid;
      # };
      # "hdmiUp".fingerprint = {
      #   HDMI1 = builtins.readFile ./hdmi1-edid;
      #   eDP1 = builtins.readFile ./eDP1-edid;
      # };
      # "laptop".fingerprint = {
      #   eDP1 = builtins.readFile ./eDP1-edid;
      # };
    };
  };
}

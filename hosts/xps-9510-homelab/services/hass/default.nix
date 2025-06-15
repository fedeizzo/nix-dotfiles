{ pkgs, config, ... }:

let
  trmnlImage = pkgs.dockerTools.buildImage {
    name = "custom-laravel";
    tag = "latest";
    fromImage = "ghcr.io/usetrmnl/byos_laravel:latest";
    config = {
      User = "82:82";
    };
  };
in
{
  services = {
    home-assistant = {
      enable = true;
      configDir = "/var/lib/hass";
      openFirewall = true;
      configWritable = false;
      extraComponents = [
        "met"
        "default_config"
        "esphome"
        "homeassistant_hardware"
        "my"
        "mqtt"
        "ffmpeg"
        "onvif" # camera
        "freebox" # router
        "prometheus"
        "http"
      ];
      extraPackages = python3Packages: with python3Packages; [
        zlib-ng
        zha
      ];
      customComponents = with pkgs; [
        home-assistant-custom-components.garmin_connect
        (pkgs.home-assistant.python.pkgs.callPackage ./custom-components/ha-bambulab.nix { })
      ];
      customLovelaceModules = with pkgs; [
        home-assistant-custom-lovelace-modules.zigbee2mqtt-networkmap
        home-assistant-custom-lovelace-modules.weather-card
        home-assistant-custom-lovelace-modules.sankey-chart
        home-assistant-custom-lovelace-modules.mushroom
        home-assistant-custom-lovelace-modules.mini-graph-card
        home-assistant-custom-lovelace-modules.bubble-card
        home-assistant-custom-lovelace-modules.button-card
        # (pkgs.callPackage ./custom-lovelace-modules/config-template-card.nix { })
        # (pkgs.callPackage ./custom-lovelace-modules/custom-ui.nix { })
        # (pkgs.callPackage ./custom-lovelace-modules/font-awesome.nix { })
        # (pkgs.callPackage ./custom-lovelace-modules/hui-element.nix { })
        # (pkgs.callPackage ./custom-lovelace-modules/lovelace-layout-card.nix { })
      ];

      config = {
        homeassistant = {
          latitude = "!secret latitude";
          longitude = "!secret longitude";
          name = "Home";
          temperature_unit = "C";
          time_zone = "Europe/Paris";
          unit_system = "metric";
        };

        http = {
          server_port = 8123;
          server_host = [ "::1" ];
          ip_ban_enabled = true;
          login_attempts_threshold = 5;
          use_x_forwarded_for = true;
          trusted_proxies = [ "::1" ];
        };

        prometheus = {
          namespace = "hass";
        };

        network = { };
        dhcp = { };

        default_config = { };
        lovelace.mode = "yaml";
      };
      lovelaceConfig = { };
    };

    zigbee2mqtt = {
      enable = true;
      dataDir = "/var/lib/zigbee2mqtt";

      settings = {
        homeassistant = config.services.home-assistant.enable;
        permit_join = false;
        serial = {
          adapter = "ember";
          port = "/dev/ttyUSB0";
        };

        mqtt = {
          server = "mqtt://localhost:1883";
        };

        frontend = {
          enabled = true;
          port = 34596;
        };

        devices = {
          "0x2c1165fffe7db3ac" = {
            friendly_name = "Air purifier";
            description = "STARKVIND Air purifier";
          };
          "0x8c65a3fffef2d632" = {
            friendly_name = "Air Quality Sensor";
            description = "VINDSTYRKA Air quality and humidity sensor (E2112)";
          };
          "0x94a081fffe5d8405" = {
            friendly_name = "3D printer plug";
            description = "INSPELNING Smart Plug (E2206)";
          };
          "0x94a081fffe5ec630" = {
            friendly_name = "Router and homelab plug";
            description = "INSPELNING Smart Plug (E2206)";
          };
          "0x94a081fffe5eb2ee" = {
            friendly_name = "IP Camera plug";
            description = "INSPELNING Smart Plug (E2206)";
          };
          "0x00158d008b3b6a70" = {
            friendly_name = "Door entrance sensor";
            description = "Aqara MCCGQ11LM";
          };
          "0x0017880108d0bb7c" = {
            friendly_name = "Hue White Ambiance GU10 Bulb 1";
            description = "Philips Hue White Ambiance GU10 with Bluetooth";
          };
          "0x0017880108cc00d5" = {
            friendly_name = "Hue White Ambiance GU10 Bulb 2";
            description = "Philips Hue White Ambiance GU10 with Bluetooth";
          };
          "0x0017880108d0b93f" = {
            friendly_name = "Hue White Ambiance GU10 Bulb 3";
            description = "Philips Hue White Ambiance GU10 with Bluetooth";
          };
        };
      };
    };

    mosquitto = {
      enable = true;
      listeners = [
        {
          acl = [ "pattern readwrite #" ];
          omitPasswordAuth = true;
          settings.allow_anonymous = true;
        }
      ];
    };
  };

  # virtualisation.oci-containers.containers.trmnl = {
  #   image = "${trmnlImage}";
  #   autoStart = true;
  #   ports = [ "34586:8080" ];
  #   user = "82:82"; # the container use a custom user
  #   environment = {
  #     PHP_OPCACHE_ENABLE = "1";
  #     TRMNL_PROXY_REFRESH_MINUTES = "15";
  #     PUID = "82";
  #     PGID = "82";
  #   };
  #   volumes = [
  #     "/var/lib/trmnl/database:/var/www/html/database"
  #     "/var/lib/trmnl/storage:/var/www/html/storage"
  #   ];
  # };

  # systemd.tmpfiles.rules = [
  #   "d /var/lib/trmnl/database 0755 82 82"
  #   "d /var/lib/trmnl/storage 0755 82 82"
  # ];
}

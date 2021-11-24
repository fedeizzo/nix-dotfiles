{ config, lib, pkgs, nixpkgs-unstable, ... }:

{
  security.rtkit.enable = true;
  services.pipewire  = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    media-session.enable = true;
    # media-session.config.bluez-monitor.rules = [
    #   {
    #     # Matches all cards
    #     matches = [ { "device.name" = "~bluez_card.*"; } ];
    #     actions = {
    #       "update-props" = {
    #         "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
    #         # mSBC is not expected to work on all headset + adapter combinations.
    #         "bluez5.msbc-support" = true;
    #         # SBC-XQ is not expected to work on all headset + adapter combinations.
    #         "bluez5.sbc-xq-support" = true;
    #       };
    #     };
    #   }
    #   {
    #     matches = [
    #       # Matches all sources
    #       { "node.name" = "~bluez_input.*"; }
    #       # Matches all outputs
    #       { "node.name" = "~bluez_output.*"; }
    #     ];
    #     actions = {
    #       "node.pause-on-idle" = false;
    #     };
    #   }
    # ];
    # config.pipewire-pulse = {
    #   "context.properties" = {
    #     "log.level" = 2;
    #   };
    #   "context.modules" = [
    #     {
    #       name = "libpipewire-module-rtkit";
    #       args = {
    #         "nice.level" = -15;
    #         "rt.prio" = 88;
    #         "rt.time.soft" = 200000;
    #         "rt.time.hard" = 200000;
    #       };
    #       flags = [ "ifexists" "nofail" ];
    #     }
    #     { name = "libpipewire-module-protocol-native"; }
    #     { name = "libpipewire-module-client-node"; }
    #     { name = "libpipewire-module-adapter"; }
    #     { name = "libpipewire-module-metadata"; }
    #     {
    #       name = "libpipewire-module-protocol-pulse";
    #       args = {
    #         "pulse.min.req" = "32/48000";
    #         "pulse.default.req" = "32/48000";
    #         "pulse.max.req" = "32/48000";
    #         "pulse.min.quantum" = "32/48000";
    #         "pulse.max.quantum" = "32/48000";
    #         "server.address" = [ "unix:native" ];
    #       };
    #     }
    #   ];
    #   "stream.properties" = {
    #     "node.latency" = "32/48000";
    #     "resample.quality" = 1;
    #   };
    # };
    # media-session.config.alsa-monitor = {
    #   rules = [
    #     {
    #       matches = [ { "node.name" = "alsa_output.*"; } ];
    #       actions = {
    #         update-props = {
    #           "audio.format" = "S32LE";
    #           "audio.rate" = 96000; # for USB soundcards it should be twice your desired rate
    #           "api.alsa.period-size" = 32; # defaults to 1024, tweak by trial-and-error
    #           #"api.alsa.disable-batch" = true; # generally, USB soundcards use the batch mode
    #         };
    #       };
    #     }
    #   ];
    # };
  };
}

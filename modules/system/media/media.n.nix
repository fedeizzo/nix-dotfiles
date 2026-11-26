{
  flake.modules.nixos.media = {
    hardware = {
      graphics = {
        enable32Bit = true;
        enable = true;
      };
    };

    location.latitude = 48.8575;
    location.longitude = 2.3514;

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      socketActivation = true;
      audio.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # WebRTC echo cancellation for the microphone, so voice calls (Open WebUI
      # realtime, video calls) don't hear their own speaker output. monitor.mode
      # takes the default sink's monitor as the echo reference, so playback needs
      # no rerouting. The high session priority makes it the default source,
      # unless a default source has been picked by hand.
      extraConfig.pipewire."60-echo-cancel" = {
        "context.modules" = [
          {
            name = "libpipewire-module-echo-cancel";
            args = {
              "library.name" = "aec/libspa-aec-webrtc";
              "monitor.mode" = true;
              "capture.props" = {
                "node.name" = "capture.echo-cancel";
                "node.passive" = true;
              };
              "source.props" = {
                "node.name" = "echo-cancel-source";
                "node.description" = "Echo Cancellation Source";
                "priority.session" = 2100;
              };
              "aec.args" = {
                "webrtc.extended_filter" = true;
                "webrtc.delay_agnostic" = true;
                "webrtc.high_pass_filter" = true;
                "webrtc.noise_suppression" = true;
                "webrtc.gain_control" = false;
              };
            };
          }
        ];
      };
      wireplumber = {
        enable = true;
        # Prevent audio playback delay on wake by disabling idle suspension for ALSA sinks
        extraConfig = {
          "10-disable-suspension" = {
            "monitor.alsa.rules" = [
              {
                matches = [ { "node.name" = "~alsa_output.*"; } ];
                actions = {
                  update-props = {
                    "session.suspend-timeout-seconds" = 0;
                  };
                };
              }
            ];
          };
        };
      };
    };
  };
}

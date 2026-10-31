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

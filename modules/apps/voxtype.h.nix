# User interactions:
# - Hold left Shift, then Fn to dictate; release either key to clean up and type the text.
# - Press Super+Shift+D or click the Voxtype widget to toggle dictation recording.
# - Press XF86NotificationCenter or Super+Ctrl+R, or right-click the Voxtype widget, to toggle a microphone meeting and save cleaned Markdown in ~/Documents/Meetings/.
# - A pulsing red dot in the bar means recording; amber means transcription is finishing.
# - Click "Copy path" in the saved-transcript notification to copy its full file path.
# - Click "Copy text" in the saved-transcript notification to copy its Markdown contents.
{
  flake-file.inputs.voxtype.url = "github:peteonrails/voxtype/v1.1.1-rc4";

  flake.modules.darwin.voxtype = { self, config, pkgs, username, ... }: {
    home-manager.users.${username}.imports = [
      self.modules.homeManager.voxtype
    ];
  };

  flake.modules.nixos.voxtype = { self, config, pkgs, username, ... }: {
    home-manager.users.${username}.imports = [
      self.modules.homeManager.voxtype
    ];
  };

  flake.modules.homeManager.voxtype = { config, pkgs, inputs, lib, ... }: let
    meetingToggle = pkgs.writeShellApplication {
      name = "voxtype-meeting";
      runtimeInputs = [ config.programs.voxtype.package pkgs.libnotify pkgs.wl-clipboard ];
      text = ''
        exec ${pkgs.python3}/bin/python3 ${./voxtype-meeting.py} ${lib.escapeShellArg "${config.home.homeDirectory}/Documents/Meetings"}
      '';
    };
  in {
    imports = [
      inputs.voxtype.homeManagerModules.default
    ];
    home.packages = [ meetingToggle ];

    programs.voxtype = {
      enable = true;
      package = inputs.voxtype.packages.${pkgs.stdenv.hostPlatform.system}.default;
      service.enable = true;
      settings = {
        # Maximum push-to-talk/toggle recording length; increase for longer dictation.
        audio.max_duration_secs = 60;
        # Recording feedback is provided by the DMS bar widget.
        osd.enabled = false;
        hotkey = {
          enabled = true;
          # Fn reports XF86WakeUp (XKB 151 = Linux evdev 143).
          # Hold Shift before Fn; releasing either key stops recording.
          key = "EVTEST_143";
          modifiers = [ "LEFTSHIFT" ];
          mode = "push_to_talk";
        };
        whisper = {
          mode = "remote";
          remote_endpoint = "https://llama.fedeizzo.dev";
          remote_model = "qwen3-asr";
          model = "qwen3-asr";
        };
        meeting = {
          enabled = true;
          storage_path = "auto";
          chunk_duration_secs = 30;
          retain_audio = false;
          audio = {
            mic_device = "default";
            loopback_device = "disabled";
            echo_cancel = "disabled";
          };
          # A single microphone cannot distinguish people by audio source.
          diarization.enabled = false;
          summary.backend = "disabled";
        };
        output = {
          mode = "type";
          fallback_to_clipboard = true;
          post_process = {
            command = "${pkgs.python3}/bin/python3 ${./voxtype-cleanup.py}";
            timeout_ms = 120000;
          };

          notification = {
            on_recording_start = false;
            on_recording_stop = false;
            on_transcription = false;
          };
        };
        text = {
          spoken_punctuation = true;
          replacements = { "vox type" = "voxtype"; };
        };
      };
    };
  };
}

{
  flake-file.inputs.voxtype.url = "github:peteonrails/voxtype/v1.0.0-rc1";

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

  flake.modules.homeManager.voxtype = { pkgs, inputs, lib, ... }: {
    imports = [
      inputs.voxtype.homeManagerModules.default
    ];

    programs.voxtype = {
      enable = true;
      package = inputs.voxtype.packages.${pkgs.system}.default;
      service.enable = true;
      settings = {
        hotkey = {
          enabled = false;
        };
        whisper = {
          mode = "remote";
          remote_endpoint = "https://llama.fedeizzo.dev";
          remote_model = "qwen3_asr";
          model = "qwen3_asr";
        };
        output = {
          mode = "type";
          fallback_to_clipboard = true;

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

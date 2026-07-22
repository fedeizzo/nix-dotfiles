{
  flake.modules.homeManager.aichat = { pkgs, lib, config, ... }:
    let
      aichat-overridden = pkgs.aichat.overrideAttrs (old: {
        postInstall = (old.postInstall or "") + ''
          install -D scripts/shell-integration/* -t $out/share/aichat/shell-integration/
          
          substituteInPlace $out/share/aichat/shell-integration/integration.bash \
            --replace-fail '"\ee"' '"\C-e"'
          substituteInPlace $out/share/aichat/shell-integration/integration.fish \
            --replace-fail 'bind \ee' 'bind \ce'
          substituteInPlace $out/share/aichat/shell-integration/integration.nu \
            --replace-fail 'modifier: alt' 'modifier: control'
          substituteInPlace $out/share/aichat/shell-integration/integration.ps1 \
            --replace-fail 'alt+e' 'ctrl+e'
          substituteInPlace $out/share/aichat/shell-integration/integration.zsh \
            --replace-fail "'\ee'" "'^e'"
        '';
      });
    in
    {
      home.packages = [ aichat-overridden ];

      xdg.configFile."aichat/config.yaml".source = (pkgs.formats.yaml { }).generate "config.yaml" {
        model = "llama:qwen-nothink";

        stream = true;
        wrap = "auto";

        clients = [{
          type = "openai-compatible";
          name = "llama";
          api_base = "https://llama.fedeizzo.dev/v1";
          models = [
            {
              name = "qwen";
              max_input_tokens = 100000;
              supports_function_calling = true;
              supports_vision = true;
            }
            {
              name = "qwen-nothink";
              max_input_tokens = 100000;
              supports_function_calling = true;
              supports_vision = true;
            }
          ];
        }];
      };
    };
}

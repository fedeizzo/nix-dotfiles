{ pkgs ? import <nixpkgs> {} }:
let
  yaml = pkgs.formats.yaml {};
in
  yaml.generate "config.yaml" {
    model = "qwen";
    stream = true;
    wrap = "auto";
    clients = [{
      type = "openai-compatible";
      name = "llama";
      api_base = "https://llama.fedeizzo.dev/v1";
      models = [{
        name = "qwen";
        max_input_tokens = 100000;
        supports_function_calling = true;
        supports_vision = true;
      }];
    }];
  }

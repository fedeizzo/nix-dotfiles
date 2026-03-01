{ pkgs-unstable, lib, ... }:

{
  services.ollama = {
    enable = true;
    host = "0.0.0.0";
    port = 11434;
    user = "ollama";
    group = "ollama";
    home = "/var/lib/ollama";
    package = pkgs-unstable.ollama-rocm;
    # acceleration = "rocm";
    rocmOverrideGfx = "11.5.1";
    loadModels = [
      # "qwen3-coder:30b"  # tool calls not working
      "qwen3:30b"
      "devstral-small-2:24b"
      "nemotron-3-nano:30b"
    ];
    syncModels = true;
    environmentVariables = {
      OLLAMA_FLASH_ATTENTION = "1";
      # OLLAMA_CONTEXT_LENGTH = "256000"; # max supported by qwen3-coder:30b
      OLLAMA_CONTEXT_LENGTH = "64000";
      # OLLAMA_KEEP_ALIVE = "-1"; # keep the model loaded
      # AMD_LOG_LEVEL = "3";
      # OLLAMA_ORIGINS = "*";
      # OLLAMA_DEBUG = "1";
      # HIP_VISIBLE_DEVICES = "1";
    };
  };

  # for consistent backup
  systemd.services.ollama.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "ollama";
    Group = "ollama";
  };
  # systemd.services.ollama-model-loader.serviceConfig = {
  #   DynamicUser = lib.mkForce false;
  #   User = "ollama";
  #   Group = "ollama";
  # };
}

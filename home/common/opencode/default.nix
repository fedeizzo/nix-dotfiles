{ inputs, pkgs, ... }:

{
  programs.opencode = {
    enable = true;
    package = inputs.opencode.packages."${pkgs.system}".opencode;
    enableMcpIntegration = true;
    # commands = {};
    # rules = '''';
    settings = {
      theme = "nord";
      plugin = [
        "@simonwjackson/opencode-direnv" # direnv loader
        # "opencode-agent-memory" # persistent memory over sessions
        "cc-safety-net" # prevents disruptive commands
      ];
      # permissions = {
      #   "my"
      # };
      provider = {
        "llama.cpp" = {
          npm = "@ai-sdk/openai-compatible";
          name = "llama-server (local)";
          options = { baseURL = "https://llama.fedeizzo.dev/v1"; };
          models = {
            qwen35 = { name = "qwen35"; limit = { context = 65536; output = 32768; }; };
          };
        };
      };
    };
  };
}

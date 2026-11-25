{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.darwin.ai-tools = { ... }: { };

  flake.modules.nixos.ai-tools = { ... }: { };

  # Alias module: light + heavy AI tooling.
  # Import ai-light alone to skip the heavy pieces (semble, codegraph).
  flake.modules.homeManager.ai-tools = { inputs, ... }: {
    imports = with inputs.self.modules.homeManager; [
      ai-light
      ai-heavy
    ];
  };
}

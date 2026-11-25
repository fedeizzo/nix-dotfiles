{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  # Heavy AI tooling: packages with big closures (semble pulls in torch).
  # Hosts that want a slim environment import ai-light instead of the ai-tools alias.
  flake.modules.homeManager.ai-heavy = { pkgs, inputs, ... }: {
    home.packages = with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
      codegraph
      semble
    ];
  };
}

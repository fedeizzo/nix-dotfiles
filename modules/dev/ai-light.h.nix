{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  # Light shared AI tooling.
  flake.modules.homeManager.ai-light = { pkgs, inputs, ... }: {
    home.packages = with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
      skills
    ];
  };
}

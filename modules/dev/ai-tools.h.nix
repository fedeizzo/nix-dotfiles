{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.darwin.ai-tools = { ... }: { };

  flake.modules.nixos.ai-tools = { ... }: { };

  flake.modules.homeManager.ai-tools = { pkgs, inputs, ... }: {
    home.packages = with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
      codegraph
      skills
      semble
    ];
  };
}

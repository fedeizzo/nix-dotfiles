{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.fence = { pkgs, lib, config, ... }: {
    home.packages = [ pkgs.llm-agents.fence ];
  };
}

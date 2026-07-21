{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.nono = { pkgs, lib, config, ... }: {
    home.packages = [ pkgs.llm-agents.nono ];
  };
}

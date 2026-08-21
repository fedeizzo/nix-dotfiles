{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.antigravity = { pkgs, lib, inputs, ... }: {
    programs.antigravity-cli = {
      enable = true;
      package = inputs.llm-agents.packages.${pkgs.system}.antigravity-cli;
      skills = builtins.mapAttrs (name: _: ../../.agents/skills/${name}) (
        lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../../.agents/skills)
      );
    };

    home.packages = [
      inputs.llm-agents.packages.${pkgs.system}.codex
      inputs.llm-agents.packages.${pkgs.system}.claude-code
      pkgs.nodejs-slim
    ];
  };
}

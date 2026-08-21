{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.claude = { pkgs, lib, inputs, ... }: {
    programs.claude-code = {
      enable = true;
      package = inputs.llm-agents.packages.${pkgs.system}.claude-code;
      skills = builtins.mapAttrs (name: _: ../../.agents/skills/${name}) (
        lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../../.agents/skills)
      );
    };
  };
}

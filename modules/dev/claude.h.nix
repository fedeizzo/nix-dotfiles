{
  flake.modules.homeManager.claude = { pkgs, lib, ... }: {
    programs.claude-code = {
      enable = true;
      package = pkgs.llm-agents.claude-code;
      skills = builtins.mapAttrs (name: _: ../../.agents/skills/${name}) (
        lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../../.agents/skills)
      );
    };
  };
}

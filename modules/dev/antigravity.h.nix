{
  flake.modules.homeManager.antigravity = { pkgs, lib, ... }: {
    programs.antigravity-cli = {
      enable = true;
      package = pkgs.llm-agents.antigravity-cli;
      skills = builtins.mapAttrs (name: _: ../../.agents/skills/${name}) (
        lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../../.agents/skills)
      );
    };
  };
}

{
  flake.modules.homeManager.ai-tools = { pkgs, lib, inputs, config, username, ... }: {
    home.packages = with pkgs.llm-agents; [
      annot
      agent-deck
      tuicr
    ];
  };
}

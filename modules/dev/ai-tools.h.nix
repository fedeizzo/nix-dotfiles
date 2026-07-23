{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.darwin.ai-tools = { pkgs, lib, inputs, config, username, ... }: {
    nixpkgs.overlays = [
      inputs.llm-agents.overlays.shared-nixpkgs
    ];
  };

  flake.modules.nixos.ai-tools = { pkgs, lib, inputs, config, username, ... }: {
    nixpkgs.overlays = [
      inputs.llm-agents.overlays.shared-nixpkgs
    ];
  };

  flake.modules.homeManager.ai-tools = { pkgs, lib, inputs, config, username, ... }: {
    home.packages = with pkgs.llm-agents; [
      codegraph
      skills
      semble
    ];
  };
}

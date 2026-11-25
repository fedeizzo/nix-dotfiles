{
  flake-file.inputs.home-manager-master.url = "github:nix-community/home-manager";
  flake-file.inputs.home-manager-master.inputs.nixpkgs.follows = "nixpkgs";

  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  # Heavy tools exposed to the pi coding agent sandbox (big closures; semble pulls in torch).
  # The pi-coding-agent program module is imported here so the option exists even when
  # pi-core is not imported; importing the same file twice is deduplicated by the module system.
  flake.modules.homeManager.pi-heavy = { pkgs, inputs, ... }: {
    imports = [
      "${inputs.home-manager-master}/modules/programs/pi-coding-agent.nix"
    ];

    programs.pi-coding-agent.extraPackages = with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
      codegraph
      semble
    ];
  };
}

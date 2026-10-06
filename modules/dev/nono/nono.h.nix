{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.nono = { pkgs, lib, config, inputs, ... }: {
    home.packages = [
      inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.nono
      inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi
      (pkgs.writeShellScriptBin "jailed-pi" ''
        exec ${inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.nono}/bin/nono run --profile pi --allow-cwd -- ${inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi}/bin/pi "$@"
      '')
    ];

    xdg.configFile."nono/profiles/pi.json" = {
      source = ./config/pi.json;
      force = true;
    };
  };
}

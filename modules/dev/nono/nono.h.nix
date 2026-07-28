{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.nono = { pkgs, lib, config, ... }: {
    home.packages = [
      pkgs.llm-agents.nono
      pkgs.llm-agents.pi
      (pkgs.writeShellScriptBin "jailed-pi" ''
        exec ${pkgs.llm-agents.nono}/bin/nono run --profile pi --allow-cwd -- ${pkgs.llm-agents.pi}/bin/pi "$@"
      '')
    ];

    xdg.configFile."nono/profiles/pi.json" = {
      source = ./config/pi.json;
      force = true;
    };
  };
}

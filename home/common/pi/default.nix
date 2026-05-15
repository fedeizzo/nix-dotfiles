{ pkgs, lib, inputs, config, username, ... }:

let
  cfg = config.programs.jail-pi;

  jailedPi = import ./jail-wrapper.nix {
    inherit pkgs username;
    persistName = cfg.persistName;
    allowNetwork = cfg.allowNetwork;
    package = pkgs.llm-agents.pi;
    jail = inputs.jail-nix.lib.init pkgs;
  };
in
{
  options.programs.jail-pi = {
    enable = lib.mkEnableOption "Jailed pi-coding-agent wrapper";

    persistName = lib.mkOption {
      type = lib.types.str;
      default = "pi";
      description = "Name used for the persistent home directory in jail.nix (~/.local/share/jail.nix/home/<name>)";
    };

    allowNetwork = lib.mkEnableOption "Allow network access inside the jail" // {
      default = true;
      description = ''
        Enables full network access for LLM API calls and plugin downloads.
        Note: bubblewrap does not support domain-level DNS filtering, so
        when enabled the jail shares the host network namespace fully.
        Disable this for maximum isolation (requires a local model server).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      # Create a convenience wrapper script that invokes the jailed pi
      (pkgs.writeShellScriptBin "pi" ''
        ${jailedPi}/bin/pi "$@"
      '')
    ];
  };
}

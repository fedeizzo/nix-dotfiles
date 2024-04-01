{ pkgs, lib, config }:

with lib;
let
  cfg = config.fedeizzo;
in
{
  options = {
    username = mkOption {
      type = types.str;
      description = "The username to use";
    };
    hostname = mkOption {
      type = types.str;
      description = "The hostname of the machine";
    };
    fs = mkOption {
      type = types.str;
      description = "Filesystem of the main disk";
    };
    system = mkOption {
      type = types.enum [ "x86_64-linux" "aarch64-linux" ];
      description = "CPU architecture";
    };
  };
  inherit config;
}

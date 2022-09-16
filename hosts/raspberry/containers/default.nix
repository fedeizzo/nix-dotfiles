{ pkgs, lib, ... }:

with lib;
{
  imports = [ ];
  options = {
    containers = mkOption {
      type = types.listOf str;
      default = [ ];
      description = "List of enabled containers";
    };
  };
}

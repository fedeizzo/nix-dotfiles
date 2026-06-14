{ inputs, lib, self, ... }:

{
  imports = [
    inputs.flake-file.flakeModules.dendritic
    inputs.flake-parts.flakeModules.modules
  ];

  flake-file = {
    description = "My personal NixOS configuration";
    outputs = "dendritic"; # uses flake-parts & import-tree to import everything in /modules
    inputs.flake-parts.url = "github:hercules-ci/flake-parts";
    inputs.import-tree.url = "github:vic/import-tree";
  };

  flake.checks = builtins.mapAttrs
    (_system: deployLib: deployLib.deployChecks self.deploy)
    inputs.deploy-rs.lib;
}

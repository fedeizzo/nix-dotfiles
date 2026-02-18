{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.fi;

  directory = types.submodule ({ config, ... }: {
    options = {
      directory = mkOption { type = type.str; };
      user = mkOption { type = types.nullOr str; };
      group = mkOption { type = types.nullOr str; };
      mode = mkOption { type = types.nullOr str; };
    };
  });

  serviceModule = types.submodule ({ config, ... }: {
    options = {
      # for the reverse proxy and the dns configuration
      name = mkOption { type = types.str; };
      subdomain = mkOption { type = types.nullOr types.str; default = config.name; };
      port = mkOption { type = types.int; };
      path = mkOption { type = types.str; default = ""; };
      isExposed = mkOption { type = types.bool; default = false; };
      authType = mkOption { type = (types.enum [ "proxy" "none" ]); default = "none"; };
      dashboardSection = mkOption { type = types.str; };
      dashboardIcon = mkOption { type = types.str; default = "${config.name}"; };
      # for impermenance and restic
      toPersist = mkOption { type = types.listOf directory; default = [ ]; };
      toBackup = mkOption { type = types.listOf types.str; default = [ ]; };
    };
  });
in
{
  options.fi = {
    services = mkOption {
      type = types.listOf serviceModule;
    };
  };
}

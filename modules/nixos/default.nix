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
    syncthing = mkOption {
      description = "Syncthing configuration";
      type = with types; submodule {
        options = {
          user = mkOption {
            type = types.str;
            description = "User used by syncthing";
          };
          folders = typesOf.listOf (submodule {
            name = mkOption {
              type = str;
              description = "Folder name";
            };
            role = mkOption {
              type = enum [ "sendreceive" "sendonly" "receiveonly" ];
              description = "Type of sharing";
            };
          });
          dataDir = mkOption {
            type = types.str;
            default = "/home/${config.syncthing.user}";
          };
          devices = types.listOf (submodule {
            options = {
              name = mkOption {
                type = types.str;
                description = "Device names";
              };
              id = mkOption {
                type = types.str;
                description = "Device IDs";
              };
              addresses = mkOption {
                type = types.listOf str;
                description = "Device addresseses";
              };
              introducer = mkOption {
                type = types.bool;
                default = false;
                description = "Device introducer flag";
              };
            };
          });
        };
      };
    };
  };
  inherit config;
}

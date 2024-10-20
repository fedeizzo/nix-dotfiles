{ inputs, homelab-configuration, ... }:

{
  deploy.nodes = {
    rasp-nixos = {
      hostname = "homelab";
      sshUser = "root";
      sudo = "doas -u";
      sshOpts = [ ];
      magicRollback = true;
      autoRollback = true;
      fastConnection = false;
      remoteBuild = false;
      profiles.system = {
        user = "root";
        path = inputs.deploy-rs.lib.aarch64-linux.activate.nixos homelab-configuration;
      };
    };
  };
}

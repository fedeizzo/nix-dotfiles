{ inputs, homelab-configuration, ... }:

{
  deploy.nodes = {
    homelab = {
      hostname = "homelab";
      sshUser = "root";
      sudo = "doas -u";
      sshOpts = [ ];
      magicRollback = true;
      autoRollback = true;
      fastConnection = false;
      remoteBuild = true;
      profiles.system = {
        user = "root";
        path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos homelab-configuration;
      };
    };
  };
}

{ inputs, homelab-configuration, rasp-configuration, ... }:

{
  deploy.nodes = {
    rasp = {
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
        path = inputs.deploy-rs.lib.aarch64-linux.activate.nixos rasp-configuration;
      };
    };

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

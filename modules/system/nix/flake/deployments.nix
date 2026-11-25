{ inputs, self, ... }:

{
  flake.deploy.nodes = {
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
        path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos
          self.nixosConfigurations.homelab;
      };
    };

    forge = {
      hostname = "192.168.1.89";
      sshUser = "root";
      sshOpts = [ ];
      magicRollback = true;
      autoRollback = true;
      fastConnection = true;
      remoteBuild = true;
      profiles.system = {
        user = "root";
        path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos
          self.nixosConfigurations.forge;
      };
    };
  };
}

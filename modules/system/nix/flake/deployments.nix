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

    datadog-workspace = {
      hostname = "workspace-federico-izzo";
      sshUser = "bits";
      # Workspace SSH aliases use short-lived multiplexed control sockets. Nix's
      # binary ssh-ng protocol must use a clean, non-TTY connection.
      sshOpts = [
        "-o" "ControlMaster=no"
        "-o" "ControlPath=none"
        "-o" "RequestTTY=no"
      ];
      remoteBuild = true;
      magicRollback = false;

      profiles.home = {
        user = "bits";
        # Preserve pre-existing workspace dotfiles on the first activation.
        path = inputs.deploy-rs.lib.aarch64-linux.activate.custom
          self.homeConfigurations."bits@datadog-workspace".activationPackage
          "HOME_MANAGER_BACKUP_EXT=hm-backup $PROFILE/activate";
      };
    };
  };
}

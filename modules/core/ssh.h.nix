{
  flake.modules.homeManager.ssh = {
    services.ssh-agent.enable = true;
    programs.ssh = {
      enable = true;
      addKeysToAgent = "yes";
      matchBlocks = {
        homelab = {
          hostname = "homelab";
          user = "root";
        };
        pikvm = {
          hostname = "kvm.lan";
          user = "root";
        };
      };
    };
  };
}

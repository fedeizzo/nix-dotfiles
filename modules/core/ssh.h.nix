{
  flake.modules.homeManager.ssh-agent = {
    services.ssh-agent.enable = true;
    programs.ssh = {
      enable = true;
      enableDefaultConfig = false;
      settings."*".AddKeysToAgent = "yes";
    };
  };

  flake.modules.homeManager.ssh = { lib, ... }: {
    programs.ssh = {
      enable = lib.mkDefault true;
      enableDefaultConfig = false;
      settings = {
        homelab = {
          Hostname = "homelab";
          User = "root";
          SetEnv = {
            TERM = "xterm-256color";
          };
        };
        mixer = {
          Hostname = "homelab";
          User = "mixer";
          SetEnv = {
            TERM = "xterm-256color";
          };
        };
        pikvm = {
          Hostname = "kvm.lan";
          User = "root";
          SetEnv = {
            TERM = "xterm-256color";
          };
        };
      };
    };
  };
}

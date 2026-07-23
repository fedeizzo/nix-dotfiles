{
  flake.modules.homeManager.ssh = {
    services.ssh-agent.enable = true;
    programs.ssh = {
      enable = true;
      addKeysToAgent = "yes";
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

_:

{
  services.ssh-agent.enable = true;
  programs.ssh = {
    enable = true;
    addKeysToAgent = "yes";
    matchBlocks = {
      homelab = {
        hostname = "homelab";
        user = "root";
      };
    };
    matchBlocks = {
      pikvm = {
        hostname = "kvm.lan";
        user = "root";
      };
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  nix.registry = {
    fedeizzo = {
      from = {
        id = "fedeizzo";
        type = "indirect";
      };
      to = {
        owner = "fedeizzo";
        repo = "nix-dotfiles";
        type = "github";
      };
    };
  };
}

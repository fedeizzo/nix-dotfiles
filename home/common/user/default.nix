_:

{
  programs.ssh = {
    enable = true;
    matchBlocks = {
      homelab = {
        hostname = "homelab";
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

{
  flake.modules.homeManager.nix-registry = {
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
  };
}

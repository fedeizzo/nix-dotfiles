{
  flake.modules.darwin.nh = { username, ... }: {
    programs.nh = {
      enable = true;
      clean.enable = true;
      clean.extraArgs = "--keep-since 4d --keep 3";
      flake = "/Users/${username}/nix-dotfiles";
    };
  };
}

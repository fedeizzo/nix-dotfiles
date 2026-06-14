{
  flake-file.inputs.nh.url = "github:nix-community/nh";

  flake.modules.nixos.nh = { username, ... }: {
    programs.nh = {
      enable = true;
      clean.enable = true;
      clean.extraArgs = "--keep-since 4d --keep 3";
      flake = "/home/${username}/nix-dotfiles";
    };
  };
}

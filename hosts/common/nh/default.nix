{ username, isMac, ... }:

let
  flakePath =
    if isMac then "/Users/${username}/nix-dotfiles" else "/home/${username}/nix-dotfiles";
in
{
  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 4d --keep 3";
    flake = flakePath;
  };
}

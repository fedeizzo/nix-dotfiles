{ username, isMac, ... }:

let
  flakePath =
    if isMac then "/Users/${username}/nix-dotfiles" else "/home/${username}/nix-dotfiles";
in
{
  programs.nh = {
    enable = true;
    os.flake = flakePath;
  };
}

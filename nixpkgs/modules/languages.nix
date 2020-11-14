{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    python3
    llvm
    lua
    python38Packages.pandocfilters

    # lua support neovim
    bat
    ripgrep
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nodePackages.dockerfile-language-server-nodejs
    python38Packages.pyls-black
    python38Packages.python-language-server
    python38Packages.black
    haskellPackages.haskell-language-server
    ccls
  ];
}

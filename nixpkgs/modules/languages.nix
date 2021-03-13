{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    clang

    # haskell
    cabal-install

    # lua
    lua

    # python
    python38
    python38Packages.pyls-black
    python38Packages.python-language-server
    python38Packages.black
    python38Packages.debugpy

    # bash
    nodePackages.bash-language-server

    # typescript
    nodePackages.typescript-language-server

    # dockerfile
    nodePackages.dockerfile-language-server-nodejs

    # c/c++
    ccls

    # rust
    rust-analyzer
    rustfmt

    # latex
    texlive.combined.scheme-full

    # note taking
    neuron-notes
  ];
}

{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    clang

    # tree-sitter
    tree-sitter

    # haskell
    # cabal-install

    # lua
    lua

    # python
    black
    python38
    nodePackages.pyright

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

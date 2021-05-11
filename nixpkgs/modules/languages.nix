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
    python38
    python38Packages.pyls-black
    python38Packages.pyls-isort
    python38Packages.pyls-mypy
    python-lsp-server

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

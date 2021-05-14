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
    sumneko-lua-language-server

    # python
    black
    python38
    python38Packages.debugpy
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

    # nix
    nixpkgs-fmt
    nixpkgs-lint
    rnix-lsp
  ];
}

{ pkgs, pkgs-unstable, ... }:

{
  home.packages = with pkgs; [
    clang

    # tree-sitter
    tree-sitter

    # python
    black
    # see emacx.nix for further information
    python3Packages.debugpy
    python3Packages.ipython
    python3Packages.pylint
    pkgs-unstable.nodePackages.pyright

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
    lld

    # latex
    texlive.combined.scheme-full

    # plot
    gnuplot

    # nix
    nixpkgs-fmt
    nixpkgs-lint
    rnix-lsp
  ];
}

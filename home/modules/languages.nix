{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    clang

    # tree-sitter
    tree-sitter

    # python
    black
    # see emacx.nix for further information
    # python39
    python39Packages.debugpy
    python39Packages.ipython
    python39Packages.pylint

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

{ pkgs, ... }:

let
  my-python-packages = python-packages: with python-packages; [
    python-lsp-server
    rope
    flake8
    # pylsp-mypy
    pyls-isort
    pyls-flake8
    # pyls-black
    debugpy
  ];
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;
in
{
  config.home.packages = with pkgs; [
    # clang

    # tree-sitter
    tree-sitter

    # python
    # see emacx.nix for further information
    python-with-my-packages

    # bash
    nodePackages.bash-language-server

    # typescript
    nodePackages.typescript-language-server

    # json
    nodePackages_latest.vscode-json-languageserver

    # dockerfile
    nodePackages.dockerfile-language-server-nodejs

    # c/c++
    ccls

    # rust
    rust-analyzer
    rustfmt
    lld

    # java
    # jdt-language-server

    # go
    # go # disabled becasuse on the macbook it installed with brew
    # gopls # disabled becasuse on the macbook it installed with brew

    # latex
    texlive.combined.scheme-full

    # nix
    nixpkgs-fmt
    nixpkgs-lint
    rnix-lsp
  ];
}

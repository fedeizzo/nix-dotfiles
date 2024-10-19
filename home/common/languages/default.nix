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

    # latex
    texliveFull

    # lua
    lua-language-server
  ] ++
  [
    # nix
    pkgs.nixpkgs-fmt
    pkgs.nixpkgs-lint
    pkgs.nil
  ];
}

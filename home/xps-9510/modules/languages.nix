{ pkgs, pkgs-unstable, ... }:

let
  my-python-packages = python-packages: with python-packages; [
    debugpy
    ipython
    pylint
    # eaf
    pandas
    requests
    pyqt6
    sip
    qtpy
    epc
    lxml
    pyqt6-webengine # for eaf
    qrcode # eaf-file-browser
    pysocks # eaf-browser
    pymupdf # eaf-pdf-viewer
    pypinyin # eaf-file-manager
    retry # eaf-markdown-previewer
    markdown
  ];
  python-with-my-packages = pkgs-unstable.python3.withPackages my-python-packages;
in
{
  home.packages = with pkgs; [
    clang

    # tree-sitter
    tree-sitter

    # python
    black
    # see emacx.nix for further information
    python-with-my-packages
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

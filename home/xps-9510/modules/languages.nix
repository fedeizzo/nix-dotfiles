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
  imports = [ ../../common/languages.nix ];
  home.packages = with pkgs; [
    python-with-my-packages
    gnuplot
  ];
}

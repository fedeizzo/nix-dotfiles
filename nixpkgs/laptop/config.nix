{ pkgs }:

let
  sources = import ../nix/sources.nix {};
  commaDer = sources.comma.outPath + "/default.nix";
  neuron-nightlyDer = pkgs.neuron-nightly-overlay.outPath + "/default.nix";
in
{
  allowUnfree = true;
  cudaSupport = true;
  packageOverrides = pkgs: with pkgs; rec {
    bspwmbar = callPackage ../pkgs/bspwmbar.nix { };
    multilockscreen = callPackage ../pkgs/multilockscreen.nix { };
    xcmenu = callPackage ../pkgs/xcmenu.nix {};
    devour = callPackage ../pkgs/devour.nix {};
    rbw = callPackage ../pkgs/rbw.nix {};
    comma = callPackage commaDer {};
    # neuron-nightly = callPackage neuron-nightlyDer {};
    # tree-sitter-python = callPackage ../pkgs/tree-sitter-python.nix {};
    # bottom = callPackage ../pkgs/bottom.nix {};
  };
}

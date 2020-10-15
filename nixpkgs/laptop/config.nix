{
  allowUnfree = true;
  cudaSupport = true;
  packageOverrides = pkgs: with pkgs; rec {
    bspwmbar = callPackage ../pkgs/bspwmbar.nix { };
    multilockscreen = callPackage ../pkgs/multilockscreen.nix { };
    # bottom = callPackage ../pkgs/bottom.nix {};
  };
}

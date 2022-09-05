{
  description = "Desc";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    pypi-deps-db = {
      url = "github:DavHau/pypi-deps-db";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mach-nix = {
      url = "github:DavHau/mach-nix/3.5.0";
      inputs.flake-utils.follows = "flake-utils";
      inputs.pypi-deps-db.follows = "pypi-deps-db";
    };
  };

  outputs = { self, nixpkgs, flake-utils, mach-nix, ... }@attr:
    let
      requirements = (builtins.readFile ./requirements.txt);
      cleanedRequirements = (builtins.replaceStrings [ ] [ ] requirements);
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        mach = mach-nix.lib.${system};

        myPython = mach.mkPython {
          python = "python39";

          requirements = cleanedRequirements;
          packagesExtra = [
            ./.
          ];
        };
      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            myPython
          ];
        };
        defaultPackage = myPython;
      }
    );
}

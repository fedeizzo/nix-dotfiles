{
  description = "Desc";
  inputs = {
    mach-nix.url = "mach-nix/3.5.0";
  };

  outputs = { self, nixpkgs, mach-nix }:
    let
      system = "x86_64-linux";
    in
    {
      packages."${system}".default = mach-nix.lib."${system}".buildPythonPackage {
        src = ./.;
      };
      devShells."${system}".default = mach-nix.lib."${system}".mkPythonShell {
        requirements = builtins.readFile ./requirements.txt;
      };
    };
}

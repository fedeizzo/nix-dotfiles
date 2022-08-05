{
  description = "Desc";
  inputs = {
    mach-nix.url = "mach-nix/3.5.0";
  };

  outputs = { self, nixpkgs, mach-nix }:
    let
      system = "x86_64-linux";
      req = builtins.filter (builtins.isString) (builtins.split "\n" (builtins.readFile ./requirements.txt));
      isComment = x: ! builtins.isNull (builtins.match "^#.*" x);
      hasVersion = x: ! builtins.isNull (builtins.match ".*=.*" x);
      deleteHttps = x: if (builtins.isNull (builtins.match "https.*" x)) then x else "";
      deleteGit = x: if (builtins.isNull (builtins.match "git.*" x)) then x else "";
      cleanNonMachNixCompatible = x: (deleteGit (deleteHttps x));
      removeVersion = x: if ((isComment x) || (! hasVersion x)) then [ (cleanNonMachNixCompatible x) ] else (builtins.match "([^=><~]*).*" x);
      cleanedReq = builtins.concatStringsSep "\n" (builtins.concatMap removeVersion req);
    in
    {
      packages."${system}".default = mach-nix.lib."${system}".buildPythonPackage {
        src = ./.;
      };
      devShells."${system}".default = mach-nix.lib."${system}".mkPythonShell {
        requirements = builtins.readFile ./requirements.txt;
        packagesExtra = [
          ./.
        ];
      };
    };
}

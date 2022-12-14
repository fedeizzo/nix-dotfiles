{
  description = "CDP";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@attr: {
    devShell.x86_64-linux =
      let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in
      pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          avrdude
          dfu-programmer
          python3
          python3Packages.pillow
        ];
      };
  };
}

{ pkgs ? import <nixpkgs> { }
}:

pkgs.stdenv.mkDerivation rec {
  name = "k3shomelab-manager";

  src = ./src;

  buildInputs = [ pkgs.sops ];

  installPhase = ''
    mkdir -p $out/bin
    cp k3shomelab-manager $out/bin
    substituteInPlace "$out/bin/k3shomelab-manager" \
        --replace @sops@ "${pkgs.sops}/bin/sops" \
  '';
}

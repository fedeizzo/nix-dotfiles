{ pkgs ? import <nixpkgs> { }
}:
pkgs.stdenv.mkDerivation rec {
  pname = "river-tag-overlay";
  version = "master";

  src = pkgs.fetchFromSourcehut {
    owner = "~leon_plickat";
    repo = "river-tag-overlay";
    rev = "${version}";
    sha256 = "sha256-bj47/NX0AWiR0ajTxQnb6pbsYyYyrK0oVLcFKlCc/nU=";
  };

  buildInputs = with pkgs; [
    pkg-config
    # make
    gcc
    wayland-scanner
    wayland
    pixman
  ];
  buildPhase = ''
    make
  '';
  installPhase = ''
    make DESTDIR=$out PREFIX="" install
  '';
  # dontUseCmakeConfigure = true;
}

{ lib
, stdenv
, fetchFromSourcehut
, pkg-config
, cmake
, gcc
, wayland
, wayland-scanner
, pixman
}:
# with import <nixpkgs> { };

stdenv.mkDerivation rec {
  pname = "river-tag-overlay";
  version = "master";

  src = fetchFromSourcehut {
    owner = "~leon_plickat";
    repo = "river-tag-overlay";
    rev = "${version}";
    sha256 = "sha256-bj47/NX0AWiR0ajTxQnb6pbsYyYyrK0oVLcFKlCc/nU=";
  };

  buildInputs = [
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

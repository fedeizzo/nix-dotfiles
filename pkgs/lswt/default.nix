{ pkgs ? import <nixpkgs> { }
}:
pkgs.stdenv.mkDerivation rec {
  pname = "lswt";
  version = "v1.0.4";

  src = pkgs.fetchFromSourcehut {
    owner = "~leon_plickat";
    repo = "lswt";
    rev = "${version}";
    sha256 = "sha256-Orwa7sV56AeznEcq/Xj5qj4PALMxq0CI+ZnXuY4JYE0=";
  };

  buildInputs = with pkgs; [
    pkg-config
    # make
    gcc
    wayland-scanner
    wayland
  ];
  buildPhase = ''
    make
  '';
  installPhase = ''
    make DESTDIR=$out PREFIX="" install
  '';
  # dontUseCmakeConfigure = true;
}

{ stdenv, fetchFromGitHub, libX11 }:
# with import <nixpkgs> {};

let
  name = "devour";
  version = "v11.0";
in
stdenv.mkDerivation {
  name = "${name}";
  src = fetchFromGitHub {
    owner = "salman-abedin";
    repo = "${name}";
    rev = "${version}";
    sha256 = "01ng1j7p3xfg2wgl8zrf147mlk8fx1z0faavi57yya9vsmdnszpr";
  };
  buildInputs = [
    libX11
  ];
  installPhase = ''
    mkdir -p $out/bin
    make all
    mv devour $out/bin
  '';
}

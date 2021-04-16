{ stdenv, fetchFromGitHub, rustPlatform, pkgconfig, openssl}:
# with import <nixpkgs> {};

rustPlatform.buildRustPackage rec {
  pname = "rbw";
  version = "1.1.2";

  src = fetchFromGitHub {
    owner = "doy";
    repo = "${pname}";
    rev = "${version}";
    sha256 = "0fwx9s5znjg9c5xbbiy9x3q944b6yvjb8pzp9z87q1h8x3k5n8g9";
  };
  cargoHash = "sha256-WCVI+b/bo95As822tkAh0bx4WKJMYhjj7BS92Nnj18s=";

  nativeBuildInputs = [ pkgconfig openssl ];
  buildInputs = [ pkgconfig openssl ];
}

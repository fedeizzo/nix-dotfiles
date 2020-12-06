{ stdenv, fetchFromGitHub, rustPlatform, pkgconfig, openssl}:
# with import <nixpkgs> {};

rustPlatform.buildRustPackage rec {
  pname = "rbw";
  version = "0.5.1";

  src = fetchFromGitHub {
    owner = "doy";
    repo = "${pname}";
    rev = "${version}";
    sha256 = "0lrmsd7lp3bqjgkzc2y4alr16xjgng1fn384yq1i095zxnpq3b15";
  };
  cargoSha256 = "1a60x6zlvl0w5wly15hd7j29pqaifzx3kkmwsw71bv74sq5yly8m";

  nativeBuildInputs = [ pkgconfig openssl ];
  buildInputs = [ pkgconfig openssl ];
}

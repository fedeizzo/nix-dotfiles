{ stdenv, fetchFromGitHub, rustPlatform, pkgconfig }:
# with import <nixpkgs> {};

rustPlatform.buildRustPackage rec {
  pname = "bottom";
  version = "0.4.7";

  src = fetchFromGitHub {
    owner = "ClementTsang";
    repo = "${pname}";
    rev = "${version}";
    sha256 = "178z9f2z861rni8zqrp4w45jmr8g325jfgwz5765sbvvf7jhjdxc";
  };
  cargoSha256 = "10z3ycl36sfhb2d1zdxrylcygw021ackavdaf4k6zsqsrm0gmrax";

  nativeBuildInputs = stdenv.lib.optionals stdenv.isLinux [ pkgconfig ];
}

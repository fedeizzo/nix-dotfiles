{ stdenv, fetchFromGitHub, rustPlatform, pkgconfig, openssl }:
# with import <nixpkgs> {};

rustPlatform.buildRustPackage rec {
  pname = "rbw";
  version = "1.4.0";

  src = fetchFromGitHub {
    owner = "doy";
    repo = "${pname}";
    rev = "${version}";
    sha256 = "sha256-/b/SQgwKsXLAj+5HdlgdLf7yR1mz1FkQSG75spxgWxA=";
  };
  cargoHash = "sha256-BrjKUovVV6BDZXtILVC0qaAF5xzE3715u9w9OYIJFbk=";

  nativeBuildInputs = [ pkgconfig openssl ];
  buildInputs = [ pkgconfig openssl ];
}

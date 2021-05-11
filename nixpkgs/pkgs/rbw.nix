{ stdenv, fetchFromGitHub, rustPlatform, pkgconfig, openssl}:
# with import <nixpkgs> {};

rustPlatform.buildRustPackage rec {
  pname = "rbw";
  version = "1.2.0";

  src = fetchFromGitHub {
    owner = "doy";
    repo = "${pname}";
    rev = "${version}";
    sha256 = "sha256-DvFNGdDcpvRbknCHVmWmFoDbrjOjf9zhZj2t4H9bqGM=";
  };
  cargoHash = "sha256-BtH3DFAq0sZvuwssrL0E8/BdbSLMeS51Mj2Hudkq9kc=";

  nativeBuildInputs = [ pkgconfig openssl ];
  buildInputs = [ pkgconfig openssl ];
}

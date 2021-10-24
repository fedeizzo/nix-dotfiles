{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "swayhide";
  version = "v0.2.0";

  src = fetchFromGitHub {
    owner = "NomisIV";
    repo = pname;
    rev = version;
    sha256 = "sha256-yn2vXKRQKdNC69F1Ofz0Vye45pia0cXmFTlT9otUAVE=";
  };

  cargoSha256 = "sha256-YWCDPZ7tE9wnscaP2lgoCJc/UiQ/QAuIkxi/l/Ua2zY=";
}

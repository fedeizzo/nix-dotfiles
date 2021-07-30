{ stdenv, fetchFromGitHub, git, cmake }:

let
  name = "LuaFormatter";
  version = "1.3.6";
in
stdenv.mkDerivation {
  name = "${name}";
  src = fetchFromGitHub {
    owner = "Koihik";
    repo = "${name}";
    rev = "${version}";
    sha256 = "sha256-C1RW4nXhM9i24MdfLBFVwowHc8t36XAmhRvEslSbgBA=";
    fetchSubmodules = true;
  };
  installPhase = ''
    cmake .
    make
    make install
  '';
  buildInputs = [ cmake ];
}

{ lib, stdenvNoCC, fetchFromGitHub, fetchYarnDeps, yarnConfigHook, yarnBuildHook, nodejs }:

stdenvNoCC.mkDerivation rec {
  pname = "lovelace-layout-card";
  version = "2.4.6";

  src = fetchFromGitHub {
    owner = "thomasloven";
    repo = pname;
    rev = "v${version}";
    sha256 = lib.fakeSha256;
  };

  offlineCache = fetchYarnDeps {
    inherit src;
    sha256 = lib.fakeSha256;
  };

  nativeBuildInputs = [ yarnConfigHook yarnBuildHook nodejs ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp dist/* $out

    runHook postInstall
  '';
}

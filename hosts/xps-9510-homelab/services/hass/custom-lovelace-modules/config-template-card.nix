{ lib, stdenvNoCC, fetchFromGitHub, fetchYarnDeps, yarnConfigHook, yarnBuildHook, nodejs }:

stdenvNoCC.mkDerivation rec {
  pname = "config-template-card";
  version = "1.3.6-unstable";

  src = fetchFromGitHub {
    owner = "oddlama";
    repo = pname;
    rev = "517159d1d0aed3d9b7664e832d20a184c52e6e2f";
    sha256 = lib.fakeSha;
  };

  offlineCache = fetchYarnDeps {
    inherit src;
    sha256 = lib.fakeSha;
  };

  nativeBuildInputs = [ yarnConfigHook yarnBuildHook nodejs ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp dist/* $out

    runHook postInstall
  '';
}

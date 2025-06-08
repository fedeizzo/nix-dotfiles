{ lib, stdenvNoCC, fetchFromGitHub, fetchYarnDeps, yarnConfigHook, yarnBuildHook, nodejs }:

stdenvNoCC.mkDerivation rec {
  pname = "hass-fontawesome";
  version = "0.1.0"; # Replace with actual tag if available

  src = fetchFromGitHub {
    owner = "thomasloven";
    repo = pname;
    rev = "v${version}";
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

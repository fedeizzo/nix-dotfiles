{ lib, stdenvNoCC, fetchFromGitHub, fetchYarnDeps, yarnConfigHook, yarnBuildHook, nodejs }:

stdenvNoCC.mkDerivation rec {
  pname = "custom-ui";
  version = "20240118";

  src = fetchFromGitHub {
    owner = "Mariusthvdb";
    repo = pname;
    rev = version;
    sha256 = "sha256-s8vNY19kvEyhhb9OUjXGGieAcRQg2lDMWl4WEIGNxhY=";
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

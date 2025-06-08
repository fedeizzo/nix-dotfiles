{ lib, stdenvNoCC, fetchFromGitHub, fetchYarnDeps, yarnConfigHook, yarnBuildHook, nodejs }:

stdenvNoCC.mkDerivation rec {
  pname = "custom-ui";
  version = "20210508"; # Replace with accurate version or tag

  src = fetchFromGitHub {
    owner = "Mariusthvdb";
    repo = pname;
    rev = version;
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

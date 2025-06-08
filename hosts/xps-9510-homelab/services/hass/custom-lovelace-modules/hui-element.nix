{ lib, stdenvNoCC, fetchFromGitHub, fetchYarnDeps, yarnConfigHook, yarnBuildHook, nodejs }:

stdenvNoCC.mkDerivation rec {
  pname = "hui-element";
  version = "1.0.3"; # Replace with actual version or tag

  src = fetchFromGitHub {
    owner = "home-assistant-community-themes";
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

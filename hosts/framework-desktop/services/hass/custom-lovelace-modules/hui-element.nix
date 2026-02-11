{ lib, stdenvNoCC, fetchFromGitHub, fetchYarnDeps, yarnConfigHook, yarnBuildHook, nodejs }:

stdenvNoCC.mkDerivation rec {
  pname = "lovelace-hui-element";
  version = "master"; # Replace with actual version or tag

  src = fetchFromGitHub {
    owner = "thomasloven";
    repo = pname;
    rev = "${version}";
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

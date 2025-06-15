{ lib, stdenvNoCC, fetchFromGitHub, fetchNpmDeps, npmHooks, nodejs }:

stdenvNoCC.mkDerivation rec {
  pname = "hass-fontawesome";
  version = "2.2.3"; # Replace with actual tag if available

  src = fetchFromGitHub {
    owner = "thomasloven";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-yKeWqzQbEAQNfqeUgjkpWz+Ya1drDcvnk9xyMeNs+yA=";
  };

  offlineCache = fetchNpmDeps {
    inherit src;
    hash = "sha256-g38ZVnK+bm41reVPaPRqPjY3fjsGC4ACi8tpFl1IgQM=";
  };

  nativeBuildInputs = [ npmHooks.npmConfigHook nodejs ];

  buildPhase = ''
    runHook preBuild

    rm hui-element.js
    npm run build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp hui-element.js $out/

    runHook postInstall
  '';
}

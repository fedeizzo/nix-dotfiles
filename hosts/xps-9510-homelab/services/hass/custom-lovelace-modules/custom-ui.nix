{ stdenvNoCC, fetchFromGitHub }:

stdenvNoCC.mkDerivation rec {
  pname = "custom-ui";
  version = "20240118";

  src = fetchFromGitHub {
    owner = "Mariusthvdb";
    repo = pname;
    rev = version;
    sha256 = "sha256-s8vNY19kvEyhhb9OUjXGGieAcRQg2lDMWl4WEIGNxhY=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir $out
    cp -v custom-ui.js $out/${pname}.js
  '';
}

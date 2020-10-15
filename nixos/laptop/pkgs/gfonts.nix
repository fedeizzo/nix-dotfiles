{ stdenv, fetchFromGitHub, git, pkg-config }:
let
  name = "urw-base35-fonts";
  version = "20200910";
in
stdenv.mkDerivation {
  name = "${name}";
  src = fetchFromGitHub {
    owner = "ArtifexSoftware";
    repo = "${name}";
    # refs/tags/v is used in order to use tag instead of version
    rev = "${version}";
    sha256 = "119zka2050ma3cgrz5y7s5b32b7ccb9bxykhaz9wcxb87ch7j2b1";
  };
  installPhase = ''
    mkdir -p $out/share/fonts/gfonts
    install -Dt $out/share/fonts/gfonts -m644 fonts/*.otf
  '';
  buildInputs = [
    git
    pkg-config
  ];
}

{ stdenv, fetchFromGitHub, zlib, git, pkg-config, libxcb, alsaLib, xcbutil}:

let
  name = "nueron-nightly";
  version = "nightly";
in
stdenv.mkDerivation {
  name = "${name}";
  src = fetchurl {
    url = "https://github.com/srid/neuron/releases/download/${version}/neuron-${version}-linux.tar.gz";
    sha256 = "0x2g1jqygyr5wiwg4ma1nd7w4ydpy82z9gkcv8vh2v8dn3y58v5m";
  };
  src = fetchFromGitHub {
    owner = "dindon-sournois";
    repo = "${name}";
    rev = "${version}";
    sha256 = "1p8c8xmxsadpsgnifccspv991ibrjli3njxwnxk55hk8gxmq0lf1";
  };
  installPhase = ''
    mkdir -p $out/bin
    make all
    cp -f xcmenu $out/bin
    chmod 755 $out/bin/xcmenu
  '';
  buildInputs = [
    zlib
    git
    pkg-config
    libxcb
    xcbutil
  ];
}

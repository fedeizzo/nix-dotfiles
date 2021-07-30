{ stdenv, fetchFromGitHub, zlib, git, pkg-config, libxcb, alsaLib, xcbutil }:

let
  name = "xcmenu";
  version = "v0.1.0";
in
stdenv.mkDerivation {
  name = "${name}";
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

{ stdenv, fetchFromGitHub, cairo, fontconfig, harfbuzz, git, pkg-config, libxcb, alsaLib, xcbutil, xcbproto, xcbutilwm, xcbutilimage, xcbutilrenderutil }:
# with import <nixpkgs> {};

let
  name = "bspwmbar";
  version = "v0.6.1";
in

stdenv.mkDerivation {
  name = "${name}";
  src = fetchFromGitHub {
    owner = "odknt";
    repo = "${name}";
    rev = "${version}";
    sha256 = "04vywamz3v2adfj8imsnx7467gbypmy1gcn6y6ab1mqv0sd3xrcd";
  };
  installPhase = ''
    mkdir -p $out/bin
    ./configure
    sed -i 's/sans-serif/monospace/' config.h
    # sed -i 's/\/\///' config.h
    sed -i 's///' config.h
    make 
    cp bspwmbar $out/bin
  '';
  buildInputs = [
    alsaLib
    cairo
    fontconfig
    harfbuzz
    git
    pkg-config
    libxcb
    xcbutil
    xcbproto
    xcbutilwm
    xcbutilimage
    xcbutilrenderutil
  ];

  # meta = {
  #   description = "i3lock wrapper with multi-monitor support";
  #   longDescription = ''
  #     i3lock wrapper with multi-monitor support.
  #   '';
  #   homepage = "https://github.com/jeffmhubbard/${name}";
  #   license = "MIT";
  #   platforms = with stdenv.lib.platforms; linux;
  #   maintainers = [
  #     stdenv.lib.maintainers.jeffmhubbard
  #   ];
  # };
}

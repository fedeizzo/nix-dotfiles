# { stdenv, fetchFromGitHub, cairo, fontconfig, harfbuzz, git, pkg-config, libxcb, alsaLib, xcbutil, xcbproto, xcbutilwm, xcbutilimage, xcbutilrenderutil }:
with import <nixpkgs> {};

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
    rev = "refs/tags/v${version}";
    sha256 = "04vywamz3v2adfj8imsnx7467gbypmy1gcn6y6ab1mqv0sd3xrcd";
  };
  installPhase = ''
    mkdir -p /usr/share/gfonts
    install -Dt /usr/share/fonts/gfonts -m644 fonts/*.otf

    install -d /etc/fonts/conf.{avail,d}
    	for _f in fontconfig/*.conf; do
		_fn="/etc/fonts/conf.avail/69-${_f}"
		install -m644 ${_f} "${_fn}"
		ln -srt "/etc/fonts/conf.d" "${_fn}"
	done
  '';
  # buildInputs = [];

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

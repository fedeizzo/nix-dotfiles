{ stdenv, fetchFromGitHub, git, pkg-config }:
# with import <nixpkgs> {};

let
  name = "nvidia-xrun";
  version = "270b6c0fd233bfd75c635f3ed261e56082f3946c";
in
stdenv.mkDerivation {
  name = "${name}";
  src = fetchFromGitHub {
    owner = "Witko";
    repo = "${name}";
    rev = "${version}";
    sha256 = "JFstFmndtG2RDXH67n0oYE3aqRhh/CgtilVbWX0dYgk=";
  };
  installPhase = ''
    mkdir -p $out
    install -Dm 644 nvidia-xrun-pm.service "$out/etc/systemd/system/nvidia-xrun-pm.service"
	install -Dm 644 config/nvidia-xrun "$out/etc/default/nvidia-xrun"
	install -Dm 644 nvidia-xorg.conf "$out/etc/X11/nvidia-xorg.conf"
	install -Dm 644 nvidia-xinitrc "$out/etc/X11/xinit/nvidia-xinitrc"
	install -Dm 755 nvidia-xrun "$out/usr/bin/nvidia-xrun"
	install -dm 555 "$out/etc/X11/xinit/nvidia-xinitrc.d"
	install -dm 555 "$out/etc/X11/nvidia-xorg.conf.d"
  '';
  buildInputs = [
    git
    pkg-config
  ];
}

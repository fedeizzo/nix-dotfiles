{ stdenv, lib, fetchFromGitHub, makeWrapper, pkgs }:
let
  name = "multilockscreen";
  version = "v1.0.0";
in
stdenv.mkDerivation {
  name = "${name}";
  src = fetchFromGitHub {
    owner = "jeffmhubbard";
    repo = "${name}";
    rev = "${version}";
    sha256 = "0gmnrq7ibbhiwsn7mfi2r71fwm6nvhiwf4wsyz44cscm474z83p0";
  };
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    substitute ${name} ${name}.patched --replace i3lock i3lock-color
    install -Dm 755 ${name}.patched $out/bin/${name}
    wrapProgram $out/bin/${name} --prefix PATH ":" ${lib.makeBinPath [ pkgs.i3lock-color pkgs.imagemagick pkgs.xorg.xdpyinfo pkgs.bc ]}
  '';

  meta = {
    description = "i3lock wrapper with multi-monitor support";
    longDescription = ''
      i3lock wrapper with multi-monitor support.
    '';
    homepage = "https://github.com/jeffmhubbard/${name}";
    license = "MIT";
    platforms = with lib.platforms; linux;
    maintainers = [
      lib.maintainers.jeffmhubbard
    ];
  };
}

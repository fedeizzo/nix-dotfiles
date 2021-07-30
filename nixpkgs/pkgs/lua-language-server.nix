# { stdenv, fetchFromGitHub, zlib, git, pkg-config, libxcb, alsaLib, xcbutil}:
with import <nixpkgs> { };
let
  name = "lua-language-server";
  version = "79bf3e605d282cdbe425eea1b5793fc97d8404da";
in
stdenv.mkDerivation {
  name = "${name}";
  src = fetchFromGitHub {
    owner = "sumneko";
    repo = "${name}";
    rev = "${version}";
    sha256 = "074jzsg1pznkfv0fxnkda4jls1irpwn6nykvhpz8wzabwhkcmqcg";
    fetchSubmodules = true;
  };

  buildInputs = [ lua5_3 ninja clang ];

  installPhase = ''
    mkdir -p $out/src
    cp -r . $out/src
    install ninja
    ninja -C 3rd/luamake -f ninja/linux.ninja
    # ./3rd/luamake/luamake rebuild
    # cd 3rd/luamake
    # rm -rf out
    # ninja -f ninja/linux.ninja
    # cd ../..
    # ./3rd/luamake/luamake rebuild
    # cp -f lua-language-server $out/bin
    # chmod 755 $out/bin/xcmenu
  '';
}

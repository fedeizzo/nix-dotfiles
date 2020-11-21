# { stdenv, fetchFromGitHub, zlib, git, pkg-config, libxcb, alsaLib, xcbutil}:
with import <nixpkgs> {};
let
  name = "lua-language-server2";
  version = "35962bc062fb778dd4038c9d8d50ea7d601453e1";
in
stdenv.mkDerivation {
  name = "${name}";
  src = fetchFromGitHub {
    owner = "sumneko";
    repo = "${name}";
    rev = "${version}";
    sha256 = "1698c9vawb89mc6fr49vy4sigx6yqxx7c5wnaqgj57mqbvih9zi4";
    fetchSubmodules = true;
  };

  buildInputs = [ lua5_3 ninja clang ];

  buildPhase = ''
    mkdir -p $out/bin
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

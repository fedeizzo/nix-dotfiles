{ stdenv, fetchFromGitHub, tree-sitter }:

let
  name = "tree-sitter-python";
  version = "0.16.1";
in
stdenv.mkDerivation {
  name = "${name}";

  src = fetchFromGitHub {
    owner = "tree-sitter";
    repo = "${name}";
    rev = "v${version}";
    sha256 = "10z8drshzyq4dapgfdsvrqz7rpx9xd83fnkg0fxy23gc1kclvqrh";
  };
  buildInputs = [
    tree-sitter
  ];
  installPhase = ''
    mkdir -p $out/lib
    cd src
    sed 's/<tree_sitter\/parser.h>/"tree_sitter\/parser.h"/' -i parser.c
    gcc -std=c99 -fPIC parser.c -c
    gcc -shared parser.o -o $out/lib/tree-sitter-python.so
  '';
}

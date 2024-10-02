{ pkgs ? import <nixpkgs> { }
}:
pkgs.stdenv.mkDerivation rec {
  pname = "adi1090x-plymouth";
  version = "5d8817458d764bff4ff9daae94cf1bbaabf16ede";

  src = pkgs.fetchFromGitHub {
    owner = "adi1090x";
    repo = "plymouth-themes";
    rev = "${version}";
    sha256 = "sha256-e3lRgIBzDkKcWEp5yyRCzQJM6yyTjYC5XmNUZZroDuw=";
  };

  buildInputs = [
    pkgs.git
  ];

  configurePhase = ''
    mkdir -p $out/share/plymouth/themes/
  '';

  buildPhase = ''
  '';

  installPhase = ''
      cp -r pack_3/owl $out/share/plymouth/themes
    cat pack_3/owl/owl.plymouth | sed  "s@\/usr\/@$out\/@" > $out/share/plymouth/themes/owl/owl.plymouth
  '';
}

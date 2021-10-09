{ pkgs ? import <nixpkgs> { }
}:
pkgs.stdenv.mkDerivation rec {
  pname = "adi1090x-plymouth";
  version = "master";

  src = pkgs.fetchFromGitHub {
    owner = "adi1090x";
    repo = "plymouth-themes";
    rev = "${version}";
    sha256 = "sha256-VNGvA8ujwjpC2rTVZKrXni2GjfiZk7AgAn4ZB4Baj2k=";
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

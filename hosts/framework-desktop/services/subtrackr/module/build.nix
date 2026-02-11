{ pkgs ? import <nixpkgs> { } }:

pkgs.buildGoModule rec {
  pname = "subtrackr";
  version = "0.5.1";

  src = pkgs.fetchFromGitHub {
    owner = "bscott";
    repo = "subtrackr";
    rev = "main";
    sha256 = "sha256-O3frf4J6DFInW0U+02QDmS30c6NRFJWy1AGpaybc2vc=";
  };

  vendorHash = "sha256-HrcpVvDVUe6RKMF9D9xgKT4nsVen50su4KtpFupZstM=";

  nativeBuildInputs = with pkgs; [ pkg-config ];
  buildInputs = with pkgs; [ sqlite ];

  # CGO_ENABLED = 1;

  ldflags = [
    "-s"
    "-w"
    "-X subtrackr/internal/version.Version=${version}"
    "-X subtrackr/internal/version.GitCommit=unknown"
  ];

  subPackages = [ "cmd/server" ];

  postInstall = ''
    mv $out/bin/server $out/bin/subtrackr

    mkdir -p $out/share/subtrackr
    cp -r templates web $out/share/subtrackr/
  '';

  nativeCheckInputs = [ pkgs.makeWrapper ];
  postFixup = ''
    wrapProgram $out/bin/subtrackr \
      --set GIN_MODE release \
      --run "cd $out/share/subtrackr"
  '';
}

{ pkgs ? import <nixpkgs> { } }:

pkgs.buildGoModule rec {
  pname = "subtrackr";
  version = "0.5.6";

  src = pkgs.fetchFromGitHub {
    owner = "bscott";
    repo = "subtrackr";
    rev = "b34e0c7e869f97031ada251c7ba56c00794d60dc";
    sha256 = "sha256-zpv9wjvnHcNBxQRoZDKNaMMFula50dd3BysAZjA4Uc0=";
  };

  vendorHash = "sha256-SijHY0hRJAZlGGu2Ps2kYO4+MWU0pyqVcknuu2Pz3tY=";

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

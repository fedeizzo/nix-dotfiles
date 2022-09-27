{ pkgs ? import <nixpkgs> { }
}:

with pkgs;
rustPlatform.buildRustPackage {
  pname = "swhkd";
  version = "1.2.1";

  src = fetchFromGitHub {
    owner = "waycrate";
    repo = "swhkd";
    #Using this commit instead of the previous, tagged release otherwise the cargo.lock was out of sync
    rev = "19d6395cb9e0e20aea5bf938f0330fa890acd906";
    sha256 = "sha256-Q9aIwATmZNOaskosdXgUlmZMIr/V/dxd8H16B4CtPt4=";
  };

  cargoSha256 = "sha256-OsHLJ6P24u63MLpnv9zUye6Mks+oP2XiRcEORMbIL6k="; #lib.fakeSha256;

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    cp ${./swhkd.service} ./swhkd.service
    cp ${./hotkeys.sh} ./hotkeys.sh
    chmod +x ./hotkeys.sh
    install -D -m0444 -t "$out/lib/systemd/user" ./swhkd.service
    install -D -m0444 -t "$out/share/swhkd" ./hotkeys.sh
    install -D -m0444 -t "$out/share/polkit-1/actions" ./com.github.swhkd.pkexec.policy
    chmod +x "$out/share/swhkd/hotkeys.sh"
    substituteInPlace "$out/share/swhkd/hotkeys.sh" \
      --replace @runtimeShell@ "${runtimeShell}" \
      --replace @psmisc@ "${psmisc}" \
      --replace @out@ "$out"
    substituteInPlace "$out/lib/systemd/user/swhkd.service" \
      --replace @out@ "$out/share/swhkd/hotkeys.sh"
    substituteInPlace "$out/share/polkit-1/actions/com.github.swhkd.pkexec.policy" \
      --replace /usr/bin/swhkd \
        "$out/bin/swhkd"
  '';
}

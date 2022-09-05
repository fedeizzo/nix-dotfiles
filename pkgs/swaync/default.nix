{ pkgs ? import <nixpkgs> { }
}:
pkgs.stdenv.mkDerivation rec {
  pname = "swaync";
  version = "v0.3";

  src = pkgs.fetchFromGitHub {
    owner = "ErikReider";
    repo = "SwayNotificationCenter";
    rev = "${version}";
    sha256 = "sha256-gXo/V2FHkHZBRmaimqJCzi0BqS4tP9IniIlubBmK5u0=";
  };

  buildInputs = with pkgs; [
    pkg-config
    cmake
    gcc
    vala
    ninja
    meson
    gtk3
    gtk-layer-shell
    dbus
    glib
    gobject-introspection
    libgee
    json-glib
    libhandy
    fish
  ];

  configurePhase = "meson build --prefix=$out";
  buildPhase = "ninja -C build";
  installPhase = "meson install -C build";
}

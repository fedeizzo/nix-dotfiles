{ pkgs, inputs', ... }:

let
  mdbook = pkgs.callPackage inputs'.nixpkgs.nixos.modules.services.web.servers.static-web-server { };

  book = pkgs.stdenv.mkDerivation {
    pname = "nix-dotfiles-docs";
    version = "0.1.1";

    src = ../../../../docs;

    buildInputs = [ pkgs.mdbook ];

    buildPhase = ''
      mdbook build
    '';

    installPhase = ''
      mkdir $out
      cp -r book/* $out/
    '';
  };
in
{
  services.static-web-server = {
    enable = true;
    listen = "[::]:34999";
    root = book;
  };
}

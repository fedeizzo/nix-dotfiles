{
  flake.modules.nixos.nix-dotfiles-docs = { pkgs, ... }:
    let
      book = pkgs.stdenv.mkDerivation {
        pname = "nix-dotfiles-docs";
        version = "0.1.1";

        src = ../../docs;

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

      fi.services = [
        { name = "nix-dotfiles"; port = 34999; isExposed = true; dashboardSection = "Exposed"; dashboardIcon = "mkdocs"; }
      ];
    };
}

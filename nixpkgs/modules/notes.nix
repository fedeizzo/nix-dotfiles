{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    zotero
    anki
    inkscape-with-extensions
    (unstable.xournalpp.overrideAttrs (old: rec {
      vi-xournalpp = fetchFromGitHub {
        owner = "raw-bacon";
        repo = "vi-xournalpp";
        rev = "master";
        sha256 = "sha256-IjJTVW9xbsMz6C78HE/uFahcA4sXxr6nx6r5lqK4O7Y=";
      };
      doDist = true;
      distPhase = ''
        mkdir -p $out/share/xournalpp/plugins/vi-xournalpp
        cp ${vi-xournalpp.out}/* $out/share/xournalpp/plugins/vi-xournalpp
      '';
      # phases = (lib.concatLists [ old.phases pluginPhase ]);
    }))
    drawio
    geogebra

    pdf2svg
  ];
}

{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    (st.overrideAttrs (oldAttrs: rec {
      buildInputs = oldAttrs.buildInputs ++ [ harfbuzz ];
      src = fetchFromGitHub {
        owner = "fedeizzo";
        repo = "st";
        rev = "master";
        sha256 = "sha256-D6zcyXPAn7ESXYbGcPu/ApwpjlD69xCSClyTaLKy6L4=";
      };
    }))
  ];
}

{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    (st.overrideAttrs (oldAttrs: rec {
      buildInputs = oldAttrs.buildInputs ++ [ harfbuzz ];
      src = fetchFromGitHub {
        owner = "fedeizzo";
        repo = "st";
        rev = "master";
        sha256 = "sha256-GzM5RAjw9tLXHfaapdRuA+ASyKnCvibzzw76CaDG7lM=";
      };
    }))
  ];
}

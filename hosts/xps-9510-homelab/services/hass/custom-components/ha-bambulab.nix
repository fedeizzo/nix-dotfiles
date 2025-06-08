{ buildHomeAssistantComponent, fetchFromGitHub, cloudscraper, beautifulsoup4 }:

buildHomeAssistantComponent rec {
  owner = "greghesp";
  domain = "bambu_lab";
  version = "2.1.17";

  src = fetchFromGitHub {
    owner = owner;
    repo = "ha-bambulab";
    tag = "v${version}";
    hash = "sha256-6yKwPZcp29BAlOcqN1n+rSEkohuVgwa7xJW4H/5MsxM=";
  };

  dependencies = [
    cloudscraper
    beautifulsoup4
  ];
}

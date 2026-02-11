{ buildHomeAssistantComponent, fetchFromGitHub }:

buildHomeAssistantComponent rec {
  owner = "cavefire";
  domain = "openid";
  version = "1.1.8";

  src = fetchFromGitHub {
    inherit owner;
    repo = "hass-${domain}";
    tag = "${version}";
    hash = "sha256-aHb/zXi+p/hWcOtoZ8orF/MmPQrSqidk2mtPqxc9o8I=";
  };

  dependencies = [ ];
}

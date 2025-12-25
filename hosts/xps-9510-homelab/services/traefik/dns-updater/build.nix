{ buildGoModule, ... }:

buildGoModule {
  pname = "dns-updater";
  version = "1.0.0";

  src = ./.;

  vendorHash = "sha256-rpg1fAC7KH5jtAO28iNsyADnZ1H1teKlg7CHVaGLUvk=";
}

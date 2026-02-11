{ buildGoModule, ... }:

buildGoModule {
  pname = "wake-on-lan-forwardauth";
  version = "1.0.0";

  src = ./.;

  vendorHash = "sha256-wOseyYkNjNxES4grXZoGV7NsXsL1kuESqmTIsuaYqhg=";
}

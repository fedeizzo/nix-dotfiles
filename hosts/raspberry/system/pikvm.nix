{ inputs, ... }:

{
  imports = [
    inputs.pikvm.nixosModules.default
  ];

  services.kvmd = {
    enable = true;
    package = inputs.pikvm.packages.aarch64-linux.default;
    hardwareVersion = "v2-hdmi-rpi4";
    nginx.enable = false;
  };
}

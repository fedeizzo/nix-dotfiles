{ inputs, system, self, ... }:

{
  packages."${system}" = {
    raspberrySDImage = inputs.nixos-generators.nixosGenerate {
      inherit system;
      format = "sd-aarch64";
      modules = [
        ../hosts/sd-installer
      ];
      specialArgs = {
        raspberry-pi-4-hardware = inputs.nixos-hardware.nixosModules.raspberry-pi-4;
        freezer-config-to-cache = self.nixosConfigurations.freezer;
      };
    };
  };
}

{ modulesPath, inputs, ... }:

{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    (modulesPath + "/installer/scan/not-detected.nix")

    ../common/system/bluetooth.nix
    ./system/boot.nix
    ../common/system/disk.nix
    ../common/system/environment.nix
    ./system/hardware.nix
    ./system/keyboardKeymapFont.nix
    ./system/networking.nix
    ../common/system/nix.nix
    ./system/persistent.nix
    ../common/system/powerThermal.nix
    ../common/system/security.nix
    ./system/sops.nix
    ./system/user.nix
    ../common/system/videoAudio.nix
  ];
}

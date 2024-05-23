{ modulesPath, inputs, ... }:

{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    (modulesPath + "/installer/scan/not-detected.nix")

    ./system/backup.nix
    ./system/bluetooth.nix
    ./system/boot.nix
    ./system/disk.nix
    ./system/environment.nix
    ./system/hardware.nix
    ./system/keyboardKeymapFont.nix
    ./system/networking.nix
    ./system/nix.nix
    ./system/persistent.nix
    ./system/powerThermal.nix
    ./system/security.nix
    ./system/sops.nix
    ./system/user.nix
    ./system/videoAudio.nix
  ];
}

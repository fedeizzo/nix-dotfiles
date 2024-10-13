{ modulesPath, inputs, username, ... }:

{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.disko.nixosModules.disko
    inputs.nh-darwin.nixosModules.default
    (modulesPath + "/installer/scan/not-detected.nix")

    ../common/system/bluetooth.nix
    ../common/system/disk.nix
    ../common/system/environment.nix
    ../common/system/nix.nix
    ../common/system/powerThermal.nix
    ../common/system/security.nix
    ../common/system/videoAudio.nix
    ./system/boot.nix
    ./system/disko.nix
    ./system/hardware.nix
    ./system/keyboardKeymapFont.nix
    ./system/networking.nix
    ./system/persistent.nix
    ./system/sops.nix
    ./system/user.nix
    (import ../common/nh { inherit username; isMac = false; })
  ];
}

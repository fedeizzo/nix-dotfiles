{ modulesPath, inputs, username, ... }:

{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.disko.nixosModules.disko
    inputs.nh-darwin.nixosModules.default
    (modulesPath + "/installer/scan/not-detected.nix")

    ./system/bluetooth.nix
    ./system/boot.nix
    ./system/disk.nix
    ./system/disko.nix
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
    (import ../common/nh { inherit username; isMac = false; })
  ];
}

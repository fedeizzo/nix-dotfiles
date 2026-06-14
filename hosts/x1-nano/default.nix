{ modulesPath, inputs, username, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")

    inputs.self.modules.nixos.bluetooth
    inputs.self.modules.nixos.boot
    inputs.self.modules.nixos.disk
    inputs.self.modules.nixos.disko-x1-nano
    inputs.self.modules.nixos.desktop-environment
    inputs.self.modules.nixos.environment
    inputs.self.modules.nixos.hardware
    inputs.self.modules.nixos.misc
    ./system/networking.nix
    inputs.self.modules.nixos.persistent
    inputs.self.modules.nixos.power
    ./system/security.nix
    inputs.self.modules.nixos.user
    inputs.self.modules.nixos.media
    inputs.self.modules.nixos.fish
    inputs.self.modules.nixos.nh
    inputs.self.modules.nixos.nix
    inputs.self.modules.nixos.solaar
    inputs.self.modules.nixos.sops
    inputs.self.modules.nixos.tailscale
    inputs.self.modules.nixos.wireguard
  ];

  system.stateVersion = "25.05";
}

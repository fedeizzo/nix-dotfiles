{ pkgs, ... }:

{
  documentation.nixos.enable = false;
  nixpkgs.config = {
    allowUnfree = true;
    permittedInsecurePackages = [
      "aspnetcore-runtime-6.0.36"
      "aspnetcore-runtime-wrapped-6.0.36"
      "dotnet-sdk-6.0.428"
      "dotnet-sdk-wrapped-6.0.428"
    ];
    rocmSupport = true;
    hostPlatform = "x86_64-linux";
  };
  nix = {
    enable = true;
    settings.trusted-users = [ "root" "nixremote" "@wheel" ];
    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 10d";
    };
    settings = {
      experimental-features = "nix-command flakes";
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    optimise.automatic = true;
    # Free up to 1GiB whenever there is less than 100MiB left.
    extraOptions = ''
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
    '';
  };
  system.stateVersion = "24.11";
}

{
  flake.modules.nixos.framework-desktop = {
    documentation.nixos.enable = false;
    nixpkgs.config = {
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
      gc = {
        automatic = true;
        dates = "daily";
        options = "--delete-older-than 10d";
      };
      optimise.automatic = true;
      extraOptions = ''
        min-free = ${toString (100 * 1024 * 1024)}
        max-free = ${toString (1024 * 1024 * 1024)}
      '';
    };
    system.stateVersion = "24.11";
  };
}

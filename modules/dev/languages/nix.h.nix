{
  flake.modules.homeManager.languages-nix = { pkgs, ... }: {
    home.packages = with pkgs; [
      nixpkgs-fmt
      nixpkgs-lint
      nil
    ];
  };
}

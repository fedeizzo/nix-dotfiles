{
  flake.modules.homeManager.languages-c = { pkgs, ... }: {
    home.packages = with pkgs; [
      ccls
    ];
  };
}

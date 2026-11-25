{
  flake.modules.homeManager.languages-docker = { pkgs, ... }: {
    home.packages = with pkgs; [
      dockerfile-language-server
    ];
  };
}

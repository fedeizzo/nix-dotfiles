{
  flake.modules.homeManager.languages-bash = { pkgs, ... }: {
    home.packages = with pkgs; [
      bash-language-server
    ];
  };
}

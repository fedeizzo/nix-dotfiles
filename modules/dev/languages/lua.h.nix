{
  flake.modules.homeManager.languages-lua = { pkgs, ... }: {
    home.packages = with pkgs; [
      lua-language-server
    ];
  };
}

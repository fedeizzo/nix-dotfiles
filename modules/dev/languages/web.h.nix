{
  flake.modules.homeManager.languages-web = { pkgs, ... }: {
    home.packages = with pkgs; [
      typescript-language-server
      vscode-langservers-extracted
    ];
  };
}

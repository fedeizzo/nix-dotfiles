{
  flake.modules.homeManager.languages = { pkgs, ... }: {
    home.packages = with pkgs; [
      (python3.withPackages (ps: with ps; [
        python-lsp-server rope flake8 pyls-isort pyls-flake8 debugpy
      ]))
      bash-language-server
      typescript-language-server
      vscode-langservers-extracted
      dockerfile-language-server
      ccls
      lua-language-server
      nixpkgs-fmt
      nixpkgs-lint
      nil
    ];
  };
}

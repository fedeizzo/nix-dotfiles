{
  flake.modules.homeManager.languages-python = { pkgs, ... }: {
    home.packages = with pkgs; [
      (python3.withPackages (ps: with ps; [
        python-lsp-server rope flake8 pyls-isort pyls-flake8 debugpy
      ]))
    ];
  };
}

{
  flake.modules.homeManager.languages = { pkgs, lib, emacs-pkg ? null, ... }: {
    home.packages = with pkgs; [
      (python3.withPackages (ps: with ps; [
        python-lsp-server rope flake8 pyls-isort pyls-flake8 debugpy
      ]))
      nodePackages.bash-language-server
      nodePackages.typescript-language-server
      nodePackages.vscode-json-languageserver
      dockerfile-language-server
      ccls
      lua-language-server
      nixpkgs-fmt
      nixpkgs-lint
      nil
    ] ++ lib.optional (emacs-pkg != null) emacs-pkg.tree-sitter;
  };
}

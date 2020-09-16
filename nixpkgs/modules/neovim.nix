{ config, pkgs, libs, ... }:

{
  # TODO install all lsp
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    extraConfig = builtins.readFile ../../dotfiles/dot_config/nvim/init.vim;
    plugins = with pkgs.vimPlugins; [
      vim-devicons
      lightline-vim
      vim-cursorword
      # TODO accelerated-jk using niv
      vim-signature
      vim-commentary
      # TODO vim-sorround using niv
      tagbar
      # TODO axe using niv
      goyo-vim
      fzf-vim
      # TODO vim-startuptime using niv
      lightline-bufferline
      indentLine
      auto-pairs
      # TODO quick-scope using niv
      vim-fugitive
      vim-json
      # TODO maybe vim-markdown doesn't work
      vim-markdown
      # TODO sxhkd-vim using niv
      # TODO see if nim-vim is same nim.nvim
      nim-vim
      vimwiki
      # TODO maybe vim-javascript, vim-jsx and typescirpt-vim don't work
      vim-javascript
      vim-jsx
      typescript-vim
      vim-toml
      vim-nix
      # TODO nord-vim using niv
      # TODO vim-lsp using niv
      # TODO async.vim using niv
      # TODO vim-lsp-settings using niv
      # TODO asyncomplete.vim using niv
      # TODO asyncomplete-lsp.vim using niv
      # TODO vim-vsnip using niv
      # TODO vim-vsnip-integ using niv
    ];
    withNodeJs = false;
    withPython = false;
    withPython3 = false;
    withRuby = false;
  # TODO see programs.neovim.configure to run plugins on specific command
  };
}

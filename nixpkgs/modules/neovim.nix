{ config, pkgs, libs, ... }:

{
  # TODO install all lsp
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    configure = {
      customRC = builtins.readFile ../../dotfiles/dot_config/nvim/init.vim;
      plug.plugins = with pkgs.vimPlugins; [
      ];
    };
    # plugins = with pkgs.vimPlugins; [
    #   ryanoasis/vim-devicons               # icon support
    #   itchyny/lightline.vim                # status line bottom of the screen
    #   itchyny/vim-cursorword               # underline same word over cursor
    #   rhysd/accelerated-jk                 # move quickly j and k for navigation 
    #   kshenoy/vim-signature                # print mark on the left
    #   tpope/vim-commentary                 # comment everything
    #   tpope/vim-surround                   # swap surround chars
    #   majutsushi/tagbar                    # tag bar on the right
    #   macthecadillac/axe                   # async command support
    #   junegunn/goyo.vim                    # focus mode
    #   /usr/bin/fzf                         # fuzzy finder support
    #   junegunn/fzf.vim                     # fuzzy finder support
    #   dstein64/vim-startuptime             # count startup time
    #   mengelbrecht/lightline-bufferline    # adds buffer top of the screen
    #   yggdroot/indentline                  # draws indent line
    #   jiangmiao/auto-pairs                 # autoclose brackets
    #   unblevable/quick-scope               # highlight unique char in word
    #   tpope/vim-fugitive                   # better git integration

    #   # languages plugins
    #   elzr/vim-json                        # json support
    #   plasticboy/vim-markdown              # markdown support
    #   kovetskiy/sxhkd-vim                  # sxhkd support
    #   alaviss/nim.nvim                     # nim support
    #   vimwiki/vimwiki                      # vimwiki
    #   pangloss/vim-javascript              # javascript
    #   mxw/vim-jsx                          # jsx
    #   leafgarland/typescript-vim           # typescript
    #   cespare/vim-toml                     # toml
    #   LnL7/vim-nix                         # nix

    #   # colorchemes
    #   arcticicestudio/nord-vim             # colorscheme

    #   # lsp plugins 
    #   prabirshrestha/vim-lsp               # lsp support
    #   prabirshrestha/async.vim             # async lsp support
    #   mattn/vim-lsp-settings               # lsp settings preconfigured


    #   # autocomplete plugins
    #   prabirshrestha/asyncomplete.vim      # async autocomplete
    #   prabirshrestha/asyncomplete-lsp.vim  # async lsp autocomplete

    #   # snippet plugins
    #   hrsh7th/vim-vsnip                    # snippet support
    #   hrsh7th/vim-vsnip-integ              # lsp snippet support
    # ];
    # extraConfig = builtins.readFile ../../dotfiles/dot_config/nvim/init.vim;
    withNodeJs = false;
    withPython = false;
    withPython3 = false;
    withRuby = false;
  # # TODO see programs.neovim.configure to run plugins on specific command
  };
}

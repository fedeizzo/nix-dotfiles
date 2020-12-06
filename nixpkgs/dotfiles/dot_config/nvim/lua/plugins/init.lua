local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
	execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
    execute 'packadd packer.nvim'
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
    use {'wbthomason/packer.nvim', opt = true}
    -- UTILITY
    -- icon support
    use 'ryanoasis/vim-devicons'
    use 'kyazdani42/nvim-web-devicons'
    -- status line
    use 'itchyny/lightline.vim'
    -- underline same work over cursor
    use 'itchyny/vim-cursorword'
    use 'rhysd/accelerated-jk'
    -- print mark on left
    use 'kshenoy/vim-signature'
    use 'tpope/vim-commentary'
    -- buffer line above
    use 'mengelbrecht/lightline-bufferline'
    use 'yggdroot/indentline'
    use 'jiangmiao/auto-pairs'
    use 'unblevable/quick-scope'
    -- git improvement
    use 'tpope/vim-fugitive'
    -- start page
    use 'mhinz/vim-startify'
    -- note taking
    use {
        'fiatjaf/neuron.vim', requires = {
            {'junegunn/fzf.vim'},
            {'junegunn/fzf'}
        }
    }

    -- LANGUAGE SUPPORT
    use {'elzr/vim-json',                      ft='json'}
    use {'plasticboy/vim-markdown',        ft='markdown'}
    use {'kovetskiy/sxhkd-vim',               ft='sxhkd'}
    use {'alaviss/nim.nvim',                    ft='nim'}
    use {'vimwiki/vimwiki',           cmd='VimwikiIndex'}
    use {'pangloss/vim-javascript',      ft='javascript'}
    use {'mxw/vim-jsx',                  ft='javascript'}
    use {'HerringtonDarkholme/yats.vim', ft='typescript'}
    use {'cespare/vim-toml',                   ft='toml'}
    use {'LnL7/vim-nix',                        ft='nix'}
    use {'tbastos/vim-lua',                     ft='lua'} -- 'tjdevries/nlua.nvim' TODO try this one
    
    -- COLORSCHEME
    use 'arcticicestudio/nord-vim'

    -- LSP / COMLETION / DAP
    use 'neovim/nvim-lspconfig'
    use {
        'nvim-lua/completion-nvim',
        requires = {
            {'hrsh7th/vim-vsnip'},
            {'hrsh7th/vim-vsnip-integ'},
            {'hrsh7th/vim-vsnip-integ'},
            {'steelsojka/completion-buffers'},
            {'nvim-treesitter/completion-treesitter'}
        }
    }
    use 'nvim-lua/lsp-status.nvim'
    use 'mfussenegger/nvim-dap'

    -- TREESITTER
    use {
        'nvim-treesitter/nvim-treesitter',
        requires = {
            {'nvim-treesitter/nvim-treesitter-refactor'},
            {'romgrk/nvim-treesitter-context'}
        }
    }

    -- TELESCOPE
    use {
        'nvim-telescope/telescope.nvim',
        requires = {
            {'nvim-lua/popup.nvim'},
            {'nvim-lua/plenary.nvim'}
        }
    }
end)

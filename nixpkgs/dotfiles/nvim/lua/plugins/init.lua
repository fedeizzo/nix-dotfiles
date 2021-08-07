local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data') .. '/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute(
    '!git clone https://github.com/wbthomason/packer.nvim ' .. install_path
  )
  execute 'packadd packer.nvim'
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(
  function()
    use {'wbthomason/packer.nvim', opt = true}
    -- UTILITY
    use 'ryanoasis/vim-devicons' -- icon support
    use 'kyazdani42/nvim-web-devicons' -- icon support
    use {
      'hoob3rt/lualine.nvim', -- status line
      requires = {'kyazdani42/nvim-web-devicons', opt = true},
    }
    use 'itchyny/vim-cursorword' -- underline same work over cursor
    use 'rhysd/accelerated-jk' -- acccelerate up and down movment
    use 'kshenoy/vim-signature' -- print mark on the left
    use 'tpope/vim-commentary' -- comment easy portion of code
    use {
      'akinsho/nvim-bufferline.lua', -- buffer line above
      requires = 'kyazdani42/nvim-web-devicons',
    }
    use 'ggandor/lightspeed.nvim' -- easy motion between buffer
    use 'yggdroot/indentline' -- indentation
    use 'windwp/nvim-autopairs' -- auto pairings
    use 'unblevable/quick-scope' -- highlight chars during inline search
    use 'glepnir/dashboard-nvim' -- start page
    use 'oberblastmeister/neuron.nvim' -- note taking
    use {'lewis6991/gitsigns.nvim'} -- git
    use {'TimUntersberger/neogit'} -- git
    use {
      'lewis6991/spellsitter.nvim', -- humant lang spell
      config = function() require'spellsitter'.setup() end,
    }
    use 'folke/twilight.nvim' -- highlight portion of code

    -- LANGUAGE SUPPORT
    use {'elzr/vim-json', ft = 'json'}
    use {'SidOfc/mkdx', ft = 'markdown'}
    use {'alaviss/nim.nvim', ft = 'nim'}
    use {'pangloss/vim-javascript', ft = 'javascript'}
    use {'mxw/vim-jsx', ft = 'javascript'}
    use {'HerringtonDarkholme/yats.vim', ft = 'typescript'}
    use {'cespare/vim-toml', ft = 'toml'}
    use {'LnL7/vim-nix', ft = 'nix'}
    use {'tbastos/vim-lua', ft = 'lua'} -- 'tjdevries/nlua.nvim' TODO try this one
    use {'neovimhaskell/haskell-vim', ft = {'haskell', 'cabal'}}

    -- COLORSCHEME
    use 'arcticicestudio/nord-vim'

    -- LSP / COMLETION / DAP
    use 'neovim/nvim-lspconfig'
    use {
      'hrsh7th/nvim-compe',
      requires = {
        {'hrsh7th/vim-vsnip'},
        {'hrsh7th/vim-vsnip-integ'},
      },
    }
    use 'nvim-lua/lsp-status.nvim'
    use 'mhartington/formatter.nvim'
    use {
      'mfussenegger/nvim-dap',
      requires = {{'theHamsta/nvim-dap-virtual-text'}, {'rcarriga/nvim-dap-ui'}},
    }

    -- TREESITTER
    use {
      'nvim-treesitter/nvim-treesitter',
      requires = {
        {'nvim-treesitter/nvim-treesitter-refactor'},
        {'romgrk/nvim-treesitter-context'},
        {'p00f/nvim-ts-rainbow'},
      },
    }

    -- TELESCOPE
    use {
      'nvim-telescope/telescope.nvim',
      requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    }
  end
)

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
    use {
        'wbthomason/packer.nvim',
        event = "VimEnter",
        opt = true
    }
    use 'kyazdani42/nvim-web-devicons' -- icon support
    use {
      'hoob3rt/lualine.nvim', -- status line
      requires = {'kyazdani42/nvim-web-devicons', opt = true},
    }
    use 'itchyny/vim-cursorword' -- underline same work over cursor
    use 'rhysd/accelerated-jk' -- acccelerate up and down movment
    use 'ggandor/lightspeed.nvim' -- easy motion between buffer
    use 'lukas-reineke/indent-blankline.nvim'
    use 'windwp/nvim-autopairs' -- auto pairings
    use 'glepnir/dashboard-nvim' -- start page
    use 'lewis6991/gitsigns.nvim' -- git

    -- MARKDOWN
    use {'SidOfc/mkdx', ft = 'markdown'}
    -- LATEX
    use {'lervag/vimtex', ft = 'tex'}

    -- COMMENT
    use 'numToStr/Comment.nvim'

    -- COLORSCHEME
    use 'NarutoXY/nvim-highlite'

    -- KEYBINDS HELPER
    use 'folke/which-key.nvim'

    -- FORMATTER
    use 'mhartington/formatter.nvim'

    -- LSP / COMLETION / DAP
    use 'neovim/nvim-lspconfig'
    use {
      "ray-x/lsp_signature.nvim",
      after = "nvim-lspconfig",
    }
    use 'nvim-lua/lsp-status.nvim'
    use {
      'hrsh7th/nvim-cmp',
      requires = {
        {'hrsh7th/vim-vsnip'},
        {'hrsh7th/vim-vsnip-integ'},
        {'rafamadriz/friendly-snippets'},
        {'hrsh7th/cmp-buffer'},
        {'hrsh7th/cmp-latex-symbols'},
        {'hrsh7th/cmp-path'},
        {'hrsh7th/cmp-nvim-lua'},
        {'hrsh7th/cmp-vsnip'},
        {'hrsh7th/cmp-nvim-lsp'},
        {'onsails/lspkind-nvim'},
      },
    }
    use {
      'mfussenegger/nvim-dap',
      requires = {
          {'theHamsta/nvim-dap-virtual-text'},
          {'rcarriga/nvim-dap-ui'}
      },
    }

    -- DIAGNOSTIC
    use "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
    use {
      "folke/trouble.nvim",
      requires = "kyazdani42/nvim-web-devicons",
    }

    -- TREESITTER
    use {
      'nvim-treesitter/nvim-treesitter',
      requires = {
        {'nvim-treesitter/nvim-treesitter-refactor'},
        {'nvim-treesitter/nvim-treesitter-textobjects'},
        {'nvim-treesitter/nvim-tree-docs'},
        {'romgrk/nvim-treesitter-context'},
        {'p00f/nvim-ts-rainbow'},
        {'SmiteshP/nvim-gps'},
        {'folke/twilight.nvim'},
      },
    }

    -- TELESCOPE
    use {
      'nvim-telescope/telescope.nvim',
      requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    }
  end
)

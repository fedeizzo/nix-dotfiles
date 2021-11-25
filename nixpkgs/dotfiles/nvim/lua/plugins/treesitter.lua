require'nvim-treesitter.configs'.setup {
  highlight = {enable = true},

  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },

  indent = {enable = true},

  refactor = {
    highlight_definitions = {enable = false},
    highlight_current_scope = {enable = false},
    smart_rename = {enable = true, keymaps = {smart_rename = '<leader>R'}},
    navigation = {
      enable = true,
      keymaps = {
        goto_definition = 'gnd',
        list_definitions = 'gnD',
        list_definitions_toc = 'gO',
        goto_next_usage = '<a-*>',
        goto_previous_usage = '<a-#>',
      },
    },
  },

  rainbow = {enable = true},

  textobjects = {
    select = {
        enable = true,
        lookahead = false,
        keymaps = {
            ["ib"] = "@block.inner",
            ["ab"] = "@block.outer",
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@class.outer",
            ["ic"] = "@class.inner",
            ["al"] = "@loop.outer",
            ["il"] = "@loop.inner",
        },
    },
    swap = {
        enable = true,
        swap_next = {
            ["<leader>s"] = "@parameter.inner",
        },
        swap_previous = {
            ["<leader>S"] = "@parameter.inner",
        },
    },
    lsp_interop = {
        enable = true,
        border = 'none',
        peek_definition_code = {
            ["<leader>df"] = "@function.outer",
            ["<leader>dF"] = "@class.outer",
        },
    },
  },

  tree_docs = { enable = true },
}

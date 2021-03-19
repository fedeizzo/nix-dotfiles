local on_attach_ls = function(client)
  require'completion'.on_attach(client)
end

-- python
require'lspconfig'.pyls.setup{on_attach=on_attach_ls}

-- bash
require'lspconfig'.bashls.setup{on_attach=on_attach_ls}

-- c/c+=
require'lspconfig'.ccls.setup{on_attach=on_attach_ls}

-- dokcer
require'lspconfig'.dockerls.setup{on_attach=on_attach_ls}

--haskell
require'lspconfig'.hls.setup{on_attach=on_attach_ls}

--typescript/javascript
require'lspconfig'.tsserver.setup{on_attach=on_attach_ls}

--rust
require'lspconfig'.rust_analyzer.setup{on_attach=on_attach_ls}

--treesitter
require'nvim-treesitter.configs'.setup {
    highlight = { enable = false },
    incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "gnn",
          node_incremental = "grn",
          scope_incremental = "grc",
          node_decremental = "grm",
        },
    },
    indent = { enable = true },
    refactor = {
        highlight_definitions = { enable = false },
        highlight_current_scope = { enable = false },
        smart_rename = {
            enable = true,
            keymaps = {
                smart_rename = "<leader>r",
            },
        },
        navigation = {
            enable = true,
            keymaps = {
                goto_definition = "gnd",
                list_definitions = "gnD",
                list_definitions_toc = "gO",
                goto_next_usage = "<a-*>",
                goto_previous_usage = "<a-#>",
            },
        },
    },
}

--debug adapter protocol
local dap = require('dap')
dap.adapters.python = {
  type = 'executable';
  command = 'python';
  args = { '-m', 'debugpy.adapter' };
}

local cmp = require'cmp'
local lspkind = require'lspkind'

cmp.setup{
    mapping = {
        ['<S-Tab>'] = cmp.mapping.select_prev_item(),
        ['<Tab>'] = cmp.mapping.select_next_item(),
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = true,
        })
    },

    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end,
    },

    sources = {
        { name = 'buffer' },
        { name = 'path' },
        { name = 'vsnip' },
        { name = 'nvim_lsp' },
        { name = 'orgmode' }
    },

    formatting = {
        format = function(entry, vim_item)
            vim_item.kind = lspkind.presets.default[vim_item.kind]
            return vim_item
        end
    },

    documentation = {
        border = {'╭', '─', '╮', '│',  '╯', '─', '╰', '│'}
    }
}
-- TODO: fix this
-- vim.api.nvim_exec([[
--     autocmd FileType lua lua require'cmp'.setup.buffer {
--     \    sources = {
--     \        { name = 'nvim_lua'},
--     \        { name = 'buffer' },
--     \        { name = 'path' },
--     \        { name = 'vsnip' },
--     \        { name = 'nvim_lsp' }
--     \    },
--     \}]], false
-- )
-- vim.api.nvim_exec([[
--     autocmd FileType tex tex require'cmp'.setup.buffer {
--     \    sources = {
--     \        { name = 'latex_symbols'},
--     \        { name = 'path' },
--     \    },
--     \ }
--   ]], false
-- )
lspkind.init({
    with_text = true,
    preset = 'codicons',
    symbol_map = {
      Text = "",
      Method = "",
      Function = "",
      Constructor = "",
      Field = "ﰠ",
      Variable = "",
      Class = "ﴯ",
      Interface = "",
      Module = "",
      Property = "ﰠ",
      Unit = "塞",
      Value = "",
      Enum = "",
      Keyword = "",
      Snippet = "",
      Color = "",
      File = "",
      Reference = "",
      Folder = "",
      EnumMember = "",
      Constant = "",
      Struct = "פּ",
      Event = "",
      Operator = "",
      TypeParameter = ""
    },
})

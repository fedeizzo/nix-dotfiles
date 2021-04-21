require'bufferline'.setup{
    options = {
        separator_style = 'thin',
        numbers = 'ordinal',
        diagnostics = "nvim_lsp",
        diagnostics_indicator = function(count, level)
            local icon = level:match("error") and " " or ""
            return " " .. icon .. count
        end,
        mappings = true,
        tab_size = 9,
    },
    highlights = {
        fill = {
            guibg = '#2E3440',
        }
    }
}

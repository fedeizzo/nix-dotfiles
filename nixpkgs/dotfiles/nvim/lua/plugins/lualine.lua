require('lualine').setup {
    options = {
        icons_enabled = true,
        theme = 'nord',
        section_separators = { left = '', right = ''},
        disabled_filetypes = {},
        always_divide_middle = true,
    },
    sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', {
            'diagnostics',
            sources={'nvim_lsp'},
            symbols = { error = " ", warn = " ", info = " ", hint = " " },
            }
        },
        lualine_c = {'filename'},
        lualine_x = {'filetype'},
        lualine_y = {},
        lualine_z = {'location'}
    },
}

vim.opt.listchars = {
    eol = "↴",
}

require("indent_blankline").setup {
    buftype_exclude = {"terminal", "dashboard", "packer", "help"},
    filetype_exclude = {"terminal", "dashboard", "packer", "help"},
    show_end_of_line = true,
    use_treesitter = true,
}

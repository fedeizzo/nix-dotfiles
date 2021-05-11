local maps = {
    nnoremap = {
        {'<silent> <Leader>o', ":lua require'telescope.builtin'.git_files{}<CR>"};
        {'<silent> <Leader>l', ":lua require'telescope.builtin'.buffers{show_all_buffers = true}<CR>"};
        {'<silent> <Leader>s', ":lua require'telescope.builtin'.builtin{}<CR>"};
        -- {'<Leader>o', ":lua telescope-file()"};
        {'<silent> <Leader>m', ":lua require'telescope.builtin'.treesitter{}<CR>"};
        {'<silent> <Leader>g', ":lua require'telescope.builtin'.git_bcommits{}<CR>"};
        {'<silent> <Leader>f', ":lua require'telescope.builtin'.live_grep{}<CR>"};
        {'<silent> <Leader>F', ":lua require'telescope.builtin'.grep_string{}<CR>"};
    };
}
createKeymaps(maps)
require('telescope').setup{
    defaults = {
        layout_strategy = "flex"
    }
}

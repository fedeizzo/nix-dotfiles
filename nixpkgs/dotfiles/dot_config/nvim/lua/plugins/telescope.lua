local maps = {
    nnoremap = {
        {'<silent> <Leader>o', ":lua require'telescope.builtin'.git_files{}<CR>"};
        {'<silent> <Leader>t', ":lua require'telescope.builtin'.treesitter{}<CR>"};
        {'<silent> <Leader>g', ":lua require'telescope.builtin'.git_bcommits{}<CR>"};
        {'<silent> <Leader>f', ":lua require'telescope.builtin'.live_grep{}<CR>"};
        {'<silent> <Leader>F', ":lua require'telescope.builtin'.grep_string{search=true}<CR>"};
    };
}
createKeymaps(maps)

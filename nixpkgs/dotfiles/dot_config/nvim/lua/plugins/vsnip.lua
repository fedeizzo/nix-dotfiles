local maps = {
    imap = {
        {'<expr> <C-j>', "vsnip#available(1) ? '<Plug>(vsnip-expand)' : '<C-j>'"};
        {'<expr> <C-l>', "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'"};
        {'<expr> <C-j>', "vsnip#available(1) ? '<Plug>(vsnip-jump-next)' : '<Tab>'"};
        {'<expr> <C-k>', "vsnip#available(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'"};
    };
    smap = {
        {'<expr> <C-j>', "vsnip#available(1) ? '<Plug>(vsnip-jump-next)' : '<Tab>'"};
        {'<expr> <C-l>', "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'"};
        {'<expr> <C-k>', "vsnip#available(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'"};
    };
}
createKeymaps(maps)
vim.g.vsnip_snippet_dir = vim.fn.expand('~/.config/nvim/snippet')

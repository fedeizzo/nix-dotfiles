require('utils/nvim-core')
local set_options = {
    omnifunc = 'v:lua.vim.lsp.omnifunc';
}
local autogroups = {
    filetypes = {
        {'FileType', 'typescript', 'setlocal omnifunc=v:lua.vim.lsp.omnifunc'  };
    };
}
local maps = {
    nnoremap = {
        {'<silent> gd'   ,      '<cmd>lua vim.lsp.buf.declaration()<CR>'};
        {'<silent> K'    ,            '<cmd>lua vim.lsp.buf.hover()<CR>'};
        {'<silent> gD'   ,   '<cmd>lua vim.lsp.buf.implementation()<CR>'};
        {'<silent> <c-k>',   '<cmd>lua vim.lsp.buf.signature_help()<CR>'};
        {'<silent> 1gD'  ,  '<cmd>lua vim.lsp.buf.type_definition()<CR>'};
        {'<silent> g0'   ,  '<cmd>lua vim.lsp.buf.document_symbol()<CR>'};
        {'<silent> gW'   , '<cmd>lua vim.lsp.buf.workspace_symbol()<CR>'};
    };
}
setOptions(set_options)
-- createAutogroups(autogroups)   
createKeymaps(maps)

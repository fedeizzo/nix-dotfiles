local M = {}

M.on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = {noremap = true, silent = true}
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap(
    'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts
  )
  buf_set_keymap(
    'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>',
    opts
  )
  buf_set_keymap(
    'n', '<leader>wl',
    '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>',
    opts
  )
  buf_set_keymap(
    'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts
  )
  buf_set_keymap('n', '<leader>r', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap(
    'n', '<leader>Ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts
  )
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap(
    'n', '<leader>d', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>',
    opts
  )
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)

  if client.resolved_capabilities.document_formatting then
    vim.api.nvim_exec(
      [[
          augroup lsp_document_formatting
            autocmd! * <buffer>
            autocmd BufWritePost <buffer> lua vim.lsp.buf.formatting_sync()
          augroup END
        ]], false
    )
    buf_set_keymap(
      'n', '<leader>F', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts
    )
  else
    buf_set_keymap('n', '<leader>F', 'Format<CR>', opts)
  end
  if client.resolved_capabilities.document_range_formatting then
    buf_set_keymap(
      'v', '<leader>F', '<cmd>lua vim.lsp.buf.range_formatting()<CR>', opts
    )
  end

  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec(
      [[
          hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
          hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
          hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
          augroup lsp_document_highlight
            autocmd! * <buffer>
            autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
            autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
          augroup END
        ]], false
    )
  end
end

return M

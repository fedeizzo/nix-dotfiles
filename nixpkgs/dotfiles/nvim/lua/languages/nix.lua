-- LSP
require'lsp'
require'lspconfig'.rnix.setup {}

-- AUTOCOMMANDS 
local function load_autocommands()
  local autogroups = {buffers = {{'BufEnter', '*.nix', 'set filetype=nix'}}}
  createAutogroups(autogroups)
end
load_autocommands()

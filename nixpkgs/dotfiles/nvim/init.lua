local execute = vim.api.nvim_command
local fn = vim.fn

local pack_path = fn.stdpath("data") .. "/site/pack"
local fmt = string.format

function ensure (user, repo)
  local install_path = fmt("%s/packer/start/%s", pack_path, repo, repo)
  if fn.empty(fn.glob(install_path)) > 0 then
    execute(fmt("!git clone https://github.com/%s/%s %s", user, repo, install_path))
    execute(fmt("packadd %s", repo))
  end
end

ensure("wbthomason", "packer.nvim")
ensure("Olical", "aniseed")
require('aniseed.env').init(
)
-- -- PLUGINS DOWNLOAD
-- require('plugins')
--
-- -- NEOVIM CORE OPTIONS
-- require('nvim-core')
--
-- -- CUSTOM FUNCTION
-- require('scripts/automatic-output')
-- require('scripts/enumerate-python-for')
-- require('scripts/telescope-file')
-- require('scripts/notes-manager')
--
-- -- PLUGINS SETTINGS
-- require('plugins/accelerated')
-- require('plugins/comment')
-- require('plugins/dashboard')
-- require('plugins/diagnostic')
-- require('plugins/formatter')
-- require('plugins/gitsigns')
-- require('plugins/haskell-vim')
-- require('plugins/indentblankline')
-- require('plugins/init')
-- require('plugins/lightspeed')
-- require('plugins/lualine')
-- require('plugins/mkdx')
-- require('plugins/nvim-autopairs')
-- require('plugins/nvim-cmp')
-- require('plugins/nvim-dap')
-- require('plugins/telescope')
-- require('plugins/treesitter')
-- require('plugins/trouble')
-- require('plugins/twilight')
-- require('plugins/vim-json')
-- require('plugins/vimmarkdown')
-- require('plugins/vsnip')
-- require('plugins/which-key')
--
-- -- LANGUAGES SETTINGS
-- require('languages/bash')
-- require('languages/c')
-- require('languages/docker')
-- require('languages/fennel')
-- require('languages/haskell')
-- require('languages/lua')
-- require('languages/md')
-- require('languages/nix')
-- require('languages/python')
-- require('languages/rust')
-- require('languages/tex')
-- require('languages/typescript')
-- require('languages/vim')
-- require('languages/all')

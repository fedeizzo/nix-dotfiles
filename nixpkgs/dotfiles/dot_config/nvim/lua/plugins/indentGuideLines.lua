require('utils/nvim-core')

vim.g.indent_guides_auto_colors = 0
vim.g.indent_guides_enable_on_vim_startup = 0
vim.g.indent_guides_color_change_percent = 1
vim.g.indentLine_char = '▏'
local command = {
    {'hi', 'IndentGuidesOdd  ctermfg=16'};
    {'hi', 'IndentGuidesEven ctermbg=darkgrey'};
};
runCommand(command)

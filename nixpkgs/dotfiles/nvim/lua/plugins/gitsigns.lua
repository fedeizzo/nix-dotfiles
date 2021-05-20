require('gitsigns').setup {
  numhl = true,
  current_line_blame = true,
  signs = {
    add = {
      hl = 'GitSignsAdd',
      text = '',
      numhl = 'GitSignsAddNr',
      linehl = 'GitSignsAddLn',
    },
    change = {
      hl = 'GitSignsChange',
      text = '',
      numhl = 'GitSignsChangeNr',
      linehl = 'GitSignsChangeLn',
    },
    delete = {
      hl = 'GitSignsDelete',
      text = '',
      numhl = 'GitSignsDeleteNr',
      linehl = 'GitSignsDeleteLn',
    },
    topdelete = {
      hl = 'GitSignsDelete',
      text = '',
      numhl = 'GitSignsDeleteNr',
      linehl = 'GitSignsDeleteLn',
    },
    changedelete = {
      hl = 'GitSignsChange',
      text = '',
      numhl = 'GitSignsChangeNr',
      linehl = 'GitSignsChangeLn',
    },
  },
  keymaps = {
    noremap = true,
    buffer = true,

    ['n ]c'] = {
      expr = true,
      '&diff ? \']c\' : \'<cmd>lua require"gitsigns".next_hunk()<CR>\'',
    },
    ['n [c'] = {
      expr = true,
      '&diff ? \'[c\' : \'<cmd>lua require"gitsigns".prev_hunk()<CR>\'',
    },
  },
}

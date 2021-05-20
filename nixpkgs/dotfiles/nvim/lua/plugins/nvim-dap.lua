local dap = require('dap')

dap.adapters.python = {
  type = 'executable',
  command = 'python',
  args = {'-m', 'debugpy.adapter'},
}

dap.configurations.python = {
  {
    type = 'python',
    request = 'launch',
    name = 'Launch file',

    program = '${file}',
    pythonPath = 'python',
  },
}

-- Commands to add breakpoint and start debug session
vim.cmd [[command Breakpoint :lua require'dap'.toggle_breakpoint()<CR>]]
vim.cmd [[command Debug :lua require'dap'.continue()<CR>]]

-- Extensions
vim.g.dap_virtual_text = true;
require('dapui').setup(
  {
    icons = {expanded = '⯆', collapsed = '⯈', circular = '↺'},
    mappings = {expand = '<CR>', open = 'o', remove = 'd'},
    sidebar = {
      elements = {'scopes', 'stacks', 'watches'},
      width = 40,
      position = 'left',
    },
    tray = {elements = {'repl'}, height = 10, position = 'bottom'},
    floating = {max_height = nil, max_width = nil},
  }
)

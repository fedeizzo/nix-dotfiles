local dap = require('dap')

dap.adapters.python = {
  type = 'executable';
  command = 'python';
  args = { '-m', 'debugpy.adapter' };
}

dap.configurations.python = {
  {
    type = 'python';
    request = 'launch';
    name = "Launch file";

    program = "${file}";
    pythonPath = 'python';
  },
}

-- Extensions
vim.g.dap_virtual_text = 'all frames';
require("dapui").setup({
  icons = {
    expanded = "⯆",
    collapsed = "⯈",
    circular = "↺"
  },
  mappings = {
    expand = "<CR>",
    open = "o",
    remove = "d"
  },
  sidebar = {
    elements = {
      "scopes",
      "stacks",
      "watches"
    },
    width = 40,
    position = "left"
  },
  tray = {
    elements = {
      "repl"
    },
    height = 10,
    position = "bottom"
  },
  floating = {
    max_height = nil,
    max_width = nil
  }
})

local sidebar = require("sidebar-nvim")
local opts = {
    open = true,
    initial_width = 25,
    side = "right",
    sections = {
        "diagnostics",
        "todos",
        "files",
        "symbols",
        "containers",
    },
    todos = {
        icon = "",
        ignored_paths = {'~', 'data'},
        initially_closed = false,
    },
    containers = {
        icon = "",
        use_podman = true,
        attach_shell = "/bin/sh",
        show_all = true,
        interval = 5000,
    },
    symbols = {
        icon = "ƒ",
    },
    files = {
        icon = "",
        show_hidden = false,
    }
}
sidebar.setup(opts)

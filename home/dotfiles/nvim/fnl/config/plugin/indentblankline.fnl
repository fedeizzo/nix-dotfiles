(module config.plugin.indentblankline
        {autoload {core aniseed.core
                   nvim aniseed.nvim
                   indent indent_blankline}})

(set nvim.o.list true)
(set nvim.o.listchars (core.str nvim.o.listchars ",eol:↴"))

(indent.setup {:buftype_exclude  ["terminal" "dashboard" "packer" "help"]
               :filetype_exclude  ["terminal" "dashboard" "packer" "help"]
               :show_end_of_line  true
               :use_treesitter  true})

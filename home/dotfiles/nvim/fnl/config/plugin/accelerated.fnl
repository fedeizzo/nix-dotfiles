(module config.plugin.accelerated
        {autoload {nvim aniseed.nvim}})

(nvim.set_keymap :n :<Down> "<Plug>(accelerated_jk_gj)" {})
(nvim.set_keymap :n :j "<Plug>(accelerated_jk_gj)" {})
(nvim.set_keymap :n :<Up> "<Plug>(accelerated_jk_gk)" {})
(nvim.set_keymap :n :k "<Plug>(accelerated_jk_gk)" {})


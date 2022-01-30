(module config.plugin.telescope
        {autoload {nvim aniseed.nvim
                   telescope telescope}})

(nvim.set_keymap "n" "<Leader>o" ":lua require'telescope.builtin'.git_files{}<CR>" {:noremap  true :silent  true})
(nvim.set_keymap "n" "<Leader>m" ":lua require'telescope.builtin'.treesitter{}<CR>" {:noremap true :silent  true})
(nvim.set_keymap "n" "<Leader>f" ":lua require'telescope.builtin'.live_grep{}<CR>" {:noremap  true :silent  true})

(telescope.setup {:defaults  {:vimgrep_arguments  ["rg"
                                                   "--colornever"
                                                   "--no-heading"
                                                   "--with-filename"
                                                   "--line-number"
                                                   "--column"
                                                   "--smart-case"]
                              :prompt_prefix  "   "
                              :selection_caret  "  "
                              :entry_prefix  "  "
                              :selection_strategy  "reset"
                              :sorting_strategy  "descending"
                              :initial_mode  "insert"
                              :layout_strategy  "flex"
                              :file_ignore_patterns  {"%.png$"
                                                      "%.tif$"
                                                      "%.tiff$"
                                                      "%.shp$"
                                                      "%.dbf$"
                                                      "%.qix$"
                                                      "%.shx$"
                                                      "%.qpj$"
                                                      "%.prj$"
                                                      "%.cpg$"
                                                      ".ccls-cache"
                                                      "node_modules"}
                              :path_display  ["truncate"]
                              :winblend  0
                              :border  {}
                              :borderchars  ["─" "│" "─" "│" "╭" "╮" "╯" "╰"]
                              :color_devicons  true
                              :use_less  true
                              :set_env  ["COLORTERM"  "truecolor"]}})

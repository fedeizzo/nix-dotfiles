(module config.plugin.gitsigns
        {autoload {gs gitsigns}})

(gs.setup {:numhl true
           :current_line_blame true
           :signs {:add {:hl "GitSignsAdd"
                         :text ""
                         :numhl "GitSignsAddNr"
                         :linehl "GitSignsAddLn"}
                   :change {:hl "GitSignsChange"
                            :text ""
                            :numhl "GitSignsChangeNr"
                            :linehl "GitSignsChangeLn"}
                   :delete {:hl "GitSignsDelete"
                            :text ""
                            :numhl "GitSignsDeleteNr"
                            :linehl "GitSignsDeleteLn"}
                   :topdelete {:hl "GitSignsDelete"
                               :text ""
                               :numhl "GitSignsDeleteNr"
                               :linehl "GitSignsDeleteLn"}
                   :changedelete {:hl "GitSignsChange"
                                  :text ""
                                  :numhl "GitSignsChangeNr"
                                  :linehl "GitSignsChangeLn"}}})

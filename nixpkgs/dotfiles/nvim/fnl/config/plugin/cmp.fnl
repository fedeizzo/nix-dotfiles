(module config.plugin.cmp
        {autoload {nvim aniseed.nvim
                   cmp cmp
                   cmp_autopairs nvim-autopairs.completion.cmp
                   lspkind lspkind}})

(defn snippet-function [args]
  (nvim.fn.vsnip#anonymous args.body))

(lspkind.init {:with_text true
               :preset :codicons
               :symbol_map {:Text ""
                            :Method ""
                            :Function ""
                            :Constructor ""
                            :Field "ﰠ"
                            :Variable ""
                            :Class "ﴯ"
                            :Interface ""
                            :Module ""
                            :Property "ﰠ"
                            :Unit "塞"
                            :Value ""
                            :Enum ""
                            :Keyword ""
                            :Snippet ""
                            :Color ""
                            :File ""
                            :Reference ""
                            :Folder ""
                            :EnumMember ""
                            :Constant ""
                            :Struct "פּ"
                            :Event ""
                            :Operator ""
                            :TypeParameter ""}})
(cmp.setup {:mapping {:<C-d> (cmp.mapping.scroll_docs -4)
                      :<C-f> (cmp.mapping.scroll_docs 4)
                      :<C-Space> (cmp.mapping.complete)
                      :<C-e> (cmp.mapping.close)
                      :<C-y> (cmp.mapping.confirm {:behavior cmp.ConfirmBehavior.Insert
                                                   :select true})}
            :sources  [{:name  :buffer :keyword_lenght  5}
                       {:name  :path}
                       {:name  :vsnip}
                       {:name  :nvim_lsp}]
            :snippet {:expand snippet-function}
            :formatting {:format
                         (lspkind.cmp_format {:with_text true
                                              :menu {:nvim_lsp "[LSP]"
                                                     :buffer "[buf]"
                                                     :path "[path]"
                                                     :vsnip "[snip]"}})}
            :documentation {:boder ["╭" "─" "╮" "│"  "╯" "─" "╰" "│"]}
            :experimental {:ghost_text true}})
(cmp.event:on :confirm_done (cmp_autopairs.on_confirm_done {:map_char {:tex ""}}))

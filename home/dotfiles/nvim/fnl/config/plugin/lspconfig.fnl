(module config.plugin.lspconfig
        {autoload {nvim aniseed.nvim
                   lsp lspconfig
                   cmplsp cmp_nvim_lsp
                   lsp_sig lsp_signature}})

(let [capabilities (cmplsp.update_capabilities (vim.lsp.protocol.make_client_capabilities))
      on_attach (fn [client bufnr]
                  (do
                    (lsp_sig.on_attach {:bind true :handler_opts {:border :rounded :hint_prefix " "}} bufnr)
                    (nvim.buf_set_option bufnr :omnifunc "v:lua.vim.lsp.omnifunc")
                    (nvim.buf_set_keymap bufnr :n :K "<Cmd>lua vim.lsp.buf.hover()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :gd "<Cmd>lua vim.lsp.buf.definition()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :<leader>ld "<Cmd>lua vim.lsp.buf.declaration()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :<leader>lt "<cmd>lua vim.lsp.buf.type_definition()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :<leader>lh "<cmd>lua vim.lsp.buf.signature_help()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :<leader>ln "<cmd>lua vim.lsp.buf.rename()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :<leader>le "<cmd>lua vim.diagnostic.open_float()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :<leader>lq "<cmd>lua vim.diagnostic.setloclist()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :<leader>lj "<cmd>lua vim.diagnostic.goto_next()<CR>" {:noremap true})
                    (nvim.buf_set_keymap bufnr :n :<leader>lk "<cmd>lua vim.diagnostic.goto_prev()<CR>" {:noremap true})
                    (nvim.ex.autocmd :BufWritePre "<buffer>" "lua vim.lsp.buf.formatting_sync()")))]
  (do
    (lsp.bashls.setup {:on_attach on_attach :capabilities capabilities})
    (lsp.ccls.setup {:on_attach on_attach :capabilities capabilities})
    (lsp.dockerls.setup {:on_attach on_attach :capabilities capabilities})
    (lsp.hls.setup {:on_attach on_attach :capabilities capabilities})
    (lsp.rnix.setup {:on_attach on_attach :capabilities capabilities})
    (lsp.rust_analyzer.setup {:on_attach on_attach :capabilities capabilities})
    (lsp.tsserver.setup {:on_attach on_attach :capabilities capabilities})
    (lsp.clojure_lsp.setup {:on_attach on_attach
                            :capabilities capabilities
                            :filetypes [:clojure :edn :fennel]})
    (lsp.pyright.setup {:on_attach on_attach
                        :capabilities capabilities
                        :python {:analysis :strict}})))

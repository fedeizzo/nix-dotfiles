(module config.plugin.formatter
        {autoload {form formatter}})

(let [black (fn []
              {:exe :black :args ["-l 79" "-"] :stdin true})
      haskell (fn []
                {:exe :ormolu :stdin true})]
  (form.setup {:login false :filetype {:python [black] :haskell [haskell]}}))

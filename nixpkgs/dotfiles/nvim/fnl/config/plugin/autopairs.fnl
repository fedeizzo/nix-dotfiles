(module config.plugin.autopairs
        {autoload {core aniseed.core
                   nvim aniseed.nvim
                   autopairs nvim-autopairs}})

(autopairs.setup {:check_ts true})

(module config.plugin.dashboard
        {autoload {nvim aniseed.nvim}})

(set nvim.g.dashboard_default_executive :telescope)
(set nvim.g.dashboard_custom_header [" ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗"
                                     " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║"
                                     " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║"
                                     " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║"
                                     " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║"
                                     " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝"])
(set nvim.g.dashboard_custom_section
     {:find_file {:description [" Find file            "]
                  :command "Telescope find_files"}
      :find_word {:description [" Find word            "]
                  :command "Telescope live_grep"}
      :find_history {:description [" Recently opened files"]
                     :command "Telescope oldfiles"}})

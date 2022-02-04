(module config.plugin
        {autoload {core aniseed.core
                   nvim aniseed.nvim
                   packer packer}})

(defn- safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :config.plugin. name))]
    (when (not ok?)
      (print (.. "Config error: " val-or-err)))))

(defn- use [...]
  (let [pkgs [...]]
    (packer.startup
     (fn [use]
       (for [i 1 (core.count pkgs) 2]
         (let [name (. pkgs i)
               opts (. pkgs (+ i 1))]
           (-?> (. opts :mod) (safe-require-plugin-config))
           (use (core.assoc opts 1 name)))))))
  nil)

(use
 "/home/fedeizzo/personalProject/nvim-printer" {}
 ;plugin manager
 :wbthomason/packer.nvim {:event :VimEnter}
 :kyazdani42/nvim-web-devicons {}
 ;acccelerate up and down movment
 :rhysd/accelerated-jk {:mod :accelerated}
 ;easy motion between buffer
 :ggandor/lightspeed.nvim {}
 ;vertical line for indentation
 :lukas-reineke/indent-blankline.nvim {:mod :indentblankline}
 ;auto pairings
 :windwp/nvim-autopairs {:mod :autopairs}
 ;start page
 :glepnir/dashboard-nvim {:mod :dashboard}
 ;git
 :lewis6991/gitsigns.nvim {:mod :gitsigns}
 ;comment
 :numToStr/Comment.nvim {:mod :comment}
 ;colorscheme
 :NarutoXY/nvim-highlite {}
 ;keybinds HELPER
 :folke/which-key.nvim {:mod :whichkey}
 ;formatter
 :mhartington/formatter.nvim {:mod :formatter}
 ;fennel
 :Olical/aniseed {}
 :Olical/conjure {:mod :conjure}
 ;markdown
 :SidOfc/mkdx {:ft :markdown :mod :markdown}
 ;latex
 :lervag/vimtex {:ft :tex}
 ;lsp / comletion
 :neovim/nvim-lspconfig {:mod :lspconfig}
 :ray-x/lsp_signature.nvim {:after :nvim-lspconfig}
 :nvim-lua/lsp-status.nvim {}
 :hrsh7th/nvim-cmp {:requires [:hrsh7th/vim-vsnip
                               :hrsh7th/vim-vsnip-integ
                               :rafamadriz/friendly-snippets
                               :hrsh7th/cmp-buffer
                               :hrsh7th/cmp-latex-symbols
                               :hrsh7th/cmp-path
                               :hrsh7th/cmp-nvim-lua
                               :hrsh7th/cmp-vsnip
                               :hrsh7th/cmp-nvim-lsp
                               :onsails/lspkind-nvim]
                    :mod :cmp}
 ;diagnostic
 :folke/trouble.nvim {:requires :kyazdani42/nvim-web-devicons :mod :trouble}
 ;treesitter
 :nvim-treesitter/nvim-treesitter {:requires [:nvim-treesitter/nvim-treesitter-refactor
                                              :nvim-treesitter/nvim-treesitter-textobjects
                                              :nvim-treesitter/nvim-tree-docs
                                              :romgrk/nvim-treesitter-context
                                              :p00f/nvim-ts-rainbow
                                              :SmiteshP/nvim-gps
                                              :folke/twilight.nvim
                                              :nvim-treesitter/playground]
                                   :mod :treesitter}
 ;telescope
 :nvim-telescope/telescope.nvim {:requires [:nvim-lua/popup.nvim :nvim-lua/plenary.nvim]
                                 :mod :telescope})

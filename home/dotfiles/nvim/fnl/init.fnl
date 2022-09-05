(module config.init
        {require {core aniseed.core
                  nvim aniseed.nvim}})

;leader maps
(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

(let [options
      {;encoding
       :encoding "utf-8"
       :fileencoding "utf-8"
       ;wild menu
       ;:wildignore (table.concat nvim.o.wildignore ["*.a" "*.o" "*.bmp" "*.gif" "*.ico" "*.jpg" "*.png" ".DS_Store" ".git" ".hg" ".svn" "* ~" "*.swp" "*.tmp"])
       :wildmenu true
       ;indent, tab and fold
       :autoindent true
       :smartindent true
       :tabstop 4
       :shiftwidth 4
       :expandtab true
       :showtabline 0
       :foldmethod "expr"
       :foldexpr "nvim_treesitter#foldexpr()"
       :foldenable false
       ;line number
       :number true
       :relativenumber true
       ;mouse
       :mouse :a
       ;split
       :splitbelow true
       ;search highlight
       :hlsearch true
       ;spell check
       :spell false
       ;colors
       :termguicolors true
       ;autochange while substituting
       :inccommand "nosplit"}]
  (each [option value (pairs options)]
        (core.assoc nvim.o option value)))

;colorscheme
(nvim.ex.colorscheme :nord)

;clipboard yank mappings
(nvim.set_keymap :n :<Leader>y "\"+" {:noremap true :silent true})
(nvim.set_keymap :v :<Leader>y "\"+y<CR>" {:noremap true :silent true})

;save and exit mappings
(nvim.set_keymap :n :<Leader>a ":w<CR>" {:noremap true :silent true})
(nvim.set_keymap :n :<Leader>q ":q<CR>" {:noremap true :silent true})
; (nvim.set_keymap :n :<Leader>c ":lua automaticOutput()<CR>" {:noremap true :silent true})

;split movement mappings
(nvim.set_keymap :n :<C-h> ":<C-w><C-h>" {:noremap true :silent true})
(nvim.set_keymap :n :<C-j> ":<C-w><C-j>" {:noremap true :silent true})
(nvim.set_keymap :n :<C-k> ":<C-w><C-k>" {:noremap true :silent true})
(nvim.set_keymap :n :<C-l> ":<C-w><C-l>" {:noremap true :silent true})

;autocmd
(nvim.ex.autocmd :FileType :haskell "setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2")
(nvim.ex.autocmd :FileType :lua "setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2")
(nvim.ex.autocmd :FileType :typescript "setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2")
(nvim.ex.autocmd "BufEnter" "*.md" "set spell")
(nvim.ex.autocmd "BufLeave" "*.md" "set nospell")
(nvim.ex.autocmd "BufEnter" "*.nix" "set filetype=nix")
(nvim.ex.autocmd "BufNewFile" "*.py" "set autoindent")
(nvim.ex.autocmd "BufRead" "*.py" "set autoindent")

(require :config.plugin)

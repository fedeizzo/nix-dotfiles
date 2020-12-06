require('utils/nvim-core')

vim.g.mapleader = ' '

local function core_options()
    local set_options = {
    -- ENCODING
        enc            = 'utf-8';
        fenc           = 'utf-8';
        termencoding   = 'utf-8';
    -- VI COMPATIBILITY
        nocompatible   = false;
    -- PATH
        path           = { '**' };
    -- WILD MENU
        wildignore     = { '*.a', '*.o', '*.bmp', '*.gif', '*.ico', '*.jpg', '*.png', '.DS_Store', '.git', '.hg', '.svn', '*~', '*.swp', '*.tmp' };
        wildmenu       = true;
    -- INDENT
        autoindent     = true;
        smartindent    = true;
    -- TAB
        tabstop        = 4;
        shiftwidth     = 4;
        expandtab      = true;
        showtabline    = 2;
        foldmethod     = 'expr';
        foldexpr       = 'nvim_treesitter#foldexpr()';
    -- FOLD
        nofoldenable   = true;
    -- LINE NUMBER
        number         = true;
        relativenumber = true;
    -- MOUSE
        mouse          = 'a';
    -- SPLIT
        splitbelow     = true;
    -- SEARCH HIGHLIGHT
        hlsearch       = true;
    -- SPELL CHECK
        spell          = true;
        spelllang      = { 'it' };
    -- COLORSCHEME
        termguicolors  = true;
    }
    local options = {
        {'syntax', 'on'};
        {'filetype', 'plugin indent', 'on'};
        {'colorscheme', 'nord'};
        {'setlocal', 'omnifunc=v:lua.vim.lsp.omnifunc'};
    };
    setOptions(set_options)
    runCommand(options)
end

local function load_autocommands()
    local autogroups = {
        buffers = {
            {'BufWritePost', '*.vim', 'source %'                                };
            {'BufWritePost', '*.cpp', 'silent lua vim.lsp.buf.formatting_sync()'};
            {'BufWritePost', '*.c'  , 'silent lua vim.lsp.buf.formatting_sync()'};
            {'BufWritePost', '*.ts' , 'silent lua vim.lsp.buf.formatting_sync()'};
            {'BufWritePost', '*.py' , 'silent lua vim.lsp.buf.formatting_sync()'};
            {'BufWritePost', '*.hs' , 'silent lua vim.lsp.buf.formatting_sync()'};
            {'BufNewFile'  , '*.py' , 'set autoindent'                          };
            {'BufRead'     , '*.py' , 'set autoindent'                          };
        };
        filetypes = {
            {'FileType', 'typescript', 'setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2'  };
            {'FileType', 'vimwiki'   , 'silent nnoremap <silent> <Leader>d :call ToggleTask()<CR>'};
            {'FileType', 'vimwiki'   , 'silent nnoremap <silent> <Leader>b :!pdfPreview "%:p"<CR>'};
            {'FileType', 'fzf'       , 'set nonu nrnu'                                            };
            {'FileType', 'git'       , 'setlocal nospell'                                         };
            {'FileType', 'qf'       , 'setlocal nospell'                                         };
        };
    }
    createAutogroups(autogroups)   
end

local function keymaps()
    local maps = {
        imap = {
            {'jk'                 , '<Esc>zz'                    };
        }; 
        inoremap = {
            {'<Tab>'              , '<C-X><C-F>'                 };
        };
        nnoremap = {
            {'<silent> <Leader>h' , ':noh<CR>'                   };
            {'<silent> <Leader>dd', ':bd<CR>'                   };
            {'<Leader>y'          , '"+'                         };
            {'<silent> <Leader>c' , ':lua automaticOutput()<CR>' };
            {'<silent> <Leader>a' , ':w<CR>'                     };
            {'<silent> <Leader>q' , ':q<CR>'                     };
            {'<C-h>'              , '<C-w><C-h>'                 };
            {'<C-j>'              , '<C-w><C-j>'                 };
            {'<C-k>'              , '<C-w><C-k>'                 };
            {'<C-l>'              , '<C-w><C-l>'                 };
        };
        vnoremap = {
            {'<Leader>y'          , '"+y<CR>'                    };
        };
    }
    createKeymaps(maps)
end

core_options()
load_autocommands()
keymaps()

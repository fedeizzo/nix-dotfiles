" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c

let g:completion_enable_snippet = 'vim-vsnip'

let g:completion_chain_complete_list = [
    \{'complete_items': ['lsp', 'snippet', 'buffers']},
    \{'mode': '<c-p>'},
    \{'mode': '<c-n>'}
\]
let g:completion_chain_complete_list = {
			\'default' : {
			\	'default' : [
			\		{'complete_items' : ['lsp', 'snippet', 'path']},
			\		{'mode' : '<c-p>'},
			\		{'mode' : '<c-n>'}
			\	],
			\	'comment' : [],
			\	'string' : []
			\	},
			\'typescript' : [
			\	{'complete_items': ['lsp', 'snippet', 'ts', 'path']},
			\	{'mode' : '<c-p>'},
			\	{'mode' : '<c-n>'}
			\	],
			\'bash' : [
			\	{'complete_items': ['lsp', 'snippet', 'ts', 'path']},
			\	{'mode' : '<c-p>'},
			\	{'mode' : '<c-n>'}
			\	],
			\'python' : [
			\	{'complete_items': ['lsp', 'snippet', 'ts', 'path']},
			\	{'mode' : '<c-p>'},
			\	{'mode' : '<c-n>'}
			\	],
			\'lua' : [
			\	{'complete_items': ['snippet', 'ts', 'path']},
			\	{'mode' : '<c-p>'},
			\	{'mode' : '<c-n>'}
			\	],
			\}
let g:completion_auto_change_source = 1


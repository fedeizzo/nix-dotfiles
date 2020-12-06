require('utils/nvim-core')

local maps = {
    nmap = {
        {'<Leader>1', '<Plug>lightline#bufferline#go(1)'};
        {'<Leader>2', '<Plug>lightline#bufferline#go(2)'};
        {'<Leader>3', '<Plug>lightline#bufferline#go(3)'};
        {'<Leader>4', '<Plug>lightline#bufferline#go(4)'};
        {'<Leader>5', '<Plug>lightline#bufferline#go(5)'};
        {'<Leader>6', '<Plug>lightline#bufferline#go(6)'};
        {'<Leader>7', '<Plug>lightline#bufferline#go(7)'};
        {'<Leader>8', '<Plug>lightline#bufferline#go(8)'};
        {'<Leader>9', '<Plug>lightline#bufferline#go(9)'};
        {'<Leader>d1', '<Plug>lightline#bufferline#delete(1)'};
        {'<Leader>d2', '<Plug>lightline#bufferline#delete(2)'};
        {'<Leader>d3', '<Plug>lightline#bufferline#delete(3)'};
        {'<Leader>d4', '<Plug>lightline#bufferline#delete(4)'};
        {'<Leader>d5', '<Plug>lightline#bufferline#delete(5)'};
        {'<Leader>d6', '<Plug>lightline#bufferline#delete(6)'};
        {'<Leader>d7', '<Plug>lightline#bufferline#delete(7)'};
        {'<Leader>d8', '<Plug>lightline#bufferline#delete(8)'};
        {'<Leader>d9', '<Plug>lightline#bufferline#delete(9)'};
    }; 
}

createKeymaps(maps)

require('utils/nvim-core')

local maps = {
    nmap = {
        {'j', '<Plug>(accelerated_jk_gj)'};
        {'k', '<Plug>(accelerated_jk_gk)'};
    }; 
}
createKeymaps(maps)

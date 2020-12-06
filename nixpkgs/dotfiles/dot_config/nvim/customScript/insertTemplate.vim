function! InsertTemplate(templateName)
    let templateFolder = $XDG_CONFIG_HOME."/nvim/templates"
    if a:templateName == "shell"
        :execute "0r".templateFolder."/sh_header.temp"
    elseif a:templateName == "asd"
        :execute "0r".templateFolder."/cpp_labASD.temp"
    endif
endfunction
command! -nargs=1 InsertTemplate call InsertTemplate(<args>)

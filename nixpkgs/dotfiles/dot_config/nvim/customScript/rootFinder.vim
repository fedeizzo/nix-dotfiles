function! RootFolder()
    let srcDir = finddir('src', expand('%:p:h').';')
    let gitDir = finddir('.git/..', expand('%:p:h').';')
    if gitDir != ""
        return gitDir
    elseif srcDir != ""
        return srcDir
    else
        return expand('%:p:h')
    endif
endfunction

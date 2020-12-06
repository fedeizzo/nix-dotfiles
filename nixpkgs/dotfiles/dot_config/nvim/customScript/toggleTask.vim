function! ToggleTask()
    let currentLine = getline(".")
    let notDone = stridx(currentLine, '[ ]')
    let done = stridx(currentLine, '[X]')
    let dash = stridx(currentLine, '-')
    let asterisk = stridx(currentLine, '*')
    if done == -1 && notDone == -1 && (dash > -1 || asterisk > -1)
        :VimwikiToggleListItem
        exec 'normal A' . ' @created(' . strftime("%Y %b %d %X") . ')'
    elseif done > -1
        :VimwikiToggleListItem
        :.s/@done(\d\d\d\d \u\w\w \d\d \d\d:\d\d:\d\d \u\u)//
        echo "no"
    elseif notDone > -1
        :VimwikiToggleListItem
        exec 'normal A' . ' @done(' . strftime("%Y %b %d %X") . ')'
    endif
endfunction

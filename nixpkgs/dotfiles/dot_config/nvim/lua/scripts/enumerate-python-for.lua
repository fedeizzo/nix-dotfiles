function enumeratePythonFor()
    vim.cmd([[.s/for \([^ ]*\) in \([^:]*\)/for i, \1 in enumerate(\2)/]])
    vim.cmd('nohlsearch')
end

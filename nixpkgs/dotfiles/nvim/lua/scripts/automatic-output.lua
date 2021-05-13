local function get_output()
    local filetype = vim.bo.filetype
    local currentWord = vim.fn.expand('<cword>')
    local outputs = {
        python = "print(f'%s: {%s}')";
        bash = 'echo \'%s\': "%s"';
        nim = 'echo "%s: ", %s';
        typescript = "console.log('%s: ', %s)";
    }
    
    local suffix = ''
    if outputs[filetype] ~= nil then
        suffix = string.format(outputs[filetype], currentWord, currentWord) 
    end
    return suffix
end

local function get_indention(line)
    local prefix = ''
    for _=1, vim.fn.indent(line) do
        prefix = prefix .. ' '
    end
    return prefix
end

function automaticOutput()
    local currentLine = vim.api.nvim_win_get_cursor(0)[1]
    local prefix = get_indention(currentLine)
    local suffix = get_output()
    if suffix ~= '' then
        vim.fn.append(currentLine, prefix .. suffix)
    end
end

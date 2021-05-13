function setOptions(options)
    for k, v in pairs(options) do
        if v == true or v == false then
            vim.api.nvim_command('set ' .. k)
        elseif type(v) == 'table' then
            local values = ''
            for k2, v2 in pairs(v) do
                if k2 == 1 then
                    values = values .. v2
                else
                    values = values .. ',' .. v2
                end
            end
            vim.api.nvim_command('set ' .. k .. '+=' .. values)
        else
            vim.api.nvim_command('set ' .. k .. '=' .. v)
        end
    end
end

local function createAutogroup(group)
    for _, def in ipairs(group) do
        local cmd = table.concat(vim.tbl_flatten{'autocmd', def}, ' ')
        vim.api.nvim_command(cmd)
    end
end

function createAutogroups(groups)
    for groupName, definitions in pairs(groups) do
        vim.api.nvim_command('augroup '..groupName)
        vim.api.nvim_command('autocmd!')
        createAutogroup(definitions)
        vim.api.nvim_command('augroup END')
    end
end

function runCommand(commands)
    for _, cmd in ipairs(commands) do
        local cmd = table.concat(vim.tbl_flatten{cmd}, ' ')
        vim.api.nvim_command(cmd)
    end
end

local function createKeymap(mapType, keymaps)
    for _, keymap in ipairs(keymaps) do
        local cmd = table.concat(vim.tbl_flatten{mapType, keymap}, ' ')
        vim.api.nvim_command(cmd)
    end
end

function createKeymaps(maps)
    for mapType, definitions in pairs(maps) do
        createKeymap(mapType, definitions)
    end
end

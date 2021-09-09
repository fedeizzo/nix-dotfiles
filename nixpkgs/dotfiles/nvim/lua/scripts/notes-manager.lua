function update_notes_date()
  local filename = vim.api.nvim_buf_get_name(0)
  if filename.find(filename, "zettelkasten") then 
    local current_date = os.date("%Y-%m-%dT%H:%M")
    vim.cmd(
      [[%s/date: [0-9]\{4\}-[01][0-9]-[0-3][0-9]T[0-2][0-9]:[0-6][0-9]/]] .. [[date: ]] .. current_date .. [[/]]
      )
    vim.cmd [[:nohlsearch]]
  end
end

function menu(title, items, prompt)
  local content = { title .. ':' }
  local valid_keys = {}
  for _, item in ipairs(items) do
    if item.separator then
      table.insert(content, string.rep(item.separator or '-', item.length or 80))
    else
      valid_keys[item.key] = item
      table.insert(content, string.format('%s %s', item.key, item.label))
    end
  end
  prompt = prompt or 'key'
  table.insert(content, prompt .. ': ')
  vim.cmd(string.format('echon "%s"', table.concat(content, '\\n')))
  local char = vim.fn.nr2char(vim.fn.getchar())
  vim.cmd([[redraw!]])
  local entry = valid_keys[char]
  if not entry or not entry.action then
    return
  end
  return entry.action()
end

function get_zettelkasten_dirs()
  local result = vim.fn.globpath('~/zettelkasten', '*', 0, 1)
  local dirs = vim.fn.filter(result, 'isdirectory(v:val)')
  local templates = {}
  local letter_used = {}
  local dirs_to_assing = {}

  for i = 97, 122 do letter_used[string.char(i)] = false end
  -- for i = 65, 90 do letter_used[string.char(i)] = false end

  for i, dir in ipairs(dirs) do
    local last_dir = dir:match(".*/([^/]+)")
    local key = string.sub(last_dir, 1, 1)

    if not letter_used[key] then
        table.insert(templates, {
            label = last_dir,
            key = key,
            action = function ()
                vim.cmd("w")
                vim.cmd("e " .. dir)
            end,
        })
        letter_used[key] = true
    else
        table.insert(dirs_to_assing, {dir = dir, last_dir = last_dir})
    end
  end
  for k, v in pairs(letter_used) do
      if not v and table.getn(dirs_to_assing) > 0 then
        local dir = table.remove(dirs_to_assing, 1)
        table.insert(templates, {
            label = dir.last_dir,
            key = k,
            action = function ()
                vim.cmd("w")
                vim.cmd("e " .. dir.dir)
            end,
        })
      end
  end
  menu('Select a directory', templates, 'Select a key')
end

vim.api.nvim_exec([[autocmd BufWritePre *.md lua update_notes_date()]], false)
vim.api.nvim_set_keymap('n', '<Leader>z', ':lua get_zettelkasten_dirs()<CR>', {noremap = true, silent = true})

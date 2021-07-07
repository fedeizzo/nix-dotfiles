function update_notes_date()
  local current_date = os.date("%Y-%m-%dT%H:%M")
  vim.cmd(
    [[%s/date: [0-9]\{4\}-[01][0-9]-[0-3][0-9]T[0-2][0-9]:[0-6][0-9]/]] .. [[date: ]] .. current_date .. [[/]]
    )
  vim.cmd [[:nohlsearch]]
end
-- vim.cmd [[command NoteUpDate :lua update_notes_date()<CR>]]

function get_zettelkasten_dirs()
  local result = vim.fn.globpath('~/zettelkasten', '*')
  local dirs = {}
  for i in result:gmatch("[^\r\n]+") do
    -- inspect(i)
    -- local tmp = {i = vim.fn.fnameescape(i)}
    table.insert(dirs, vim.fn.expand(i, ":t"))
  end
  local dirs = vim.fn.filter(dirs, 'isdirectory(v:val)') 
  vim.fn.inputlist(dirs)
end

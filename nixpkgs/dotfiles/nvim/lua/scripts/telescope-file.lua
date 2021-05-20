function telescope_file()
  opts = otps or {}
  opts.cwd = vim.fn.systemlist('git rev-parse --show-toplevel')[1]
  local err = vim.api.nvim_get_vvar('shell_error')
  if err == 0 then
    vim.cmd(':lua require\'telescope.builtin\'.git_files{}')
  else
    vim.cmd(':lua require\'telescope.builtin\'.file_browser{}')
  end
end

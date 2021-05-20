-- AUTOCOMMANDS 
local function load_autocommands()
  local autogroups = {buffers = {{'BufWritePost', '*.vim', 'source %'}}}
  createAutogroups(autogroups)
end
load_autocommands()

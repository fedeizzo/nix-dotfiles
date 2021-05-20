-- AUTOCOMMANDS 
local function load_autocommands()
  local autogroups = {
    buffers = {
      {'BufEnter', '*.md', 'set spell'},
      {'BufLeave', '*.md', 'set nospell'},
    },
  }
  createAutogroups(autogroups)
end
load_autocommands()

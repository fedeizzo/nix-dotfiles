require('formatter').setup(
  {
    logging = false,
    filetype = {
      python = {
        -- black
        function()
          return {exe = 'black', args = {'-l 79', '-'}, stdin = true}
        end,
      },
    },
  }
)

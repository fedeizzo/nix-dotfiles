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
      haskell = {
          function()
            return {exe = 'ormolu', stdin = true}
          end
      }
    },
  }
)

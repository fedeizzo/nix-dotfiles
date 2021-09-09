-- require("org-bullets").setup {
--     symbols = { "◉", "○", "✸", "✿" }
-- }
require'orgmode'.setup({
    org_agenda_files = '~/docs/org/agenda/*',
    org_default_notes_file = '~/docs/org/notes.org',
    org_hide_leading_stars = true,
    mappings = {
        global = {
            org_agenda = '<Leader>la',
            org_capture = '<Leader>lc',
        },
    }
})
-- vim.cmd[[
--     autocmd ColorScheme * call s:setup_org_colors()

--     function! s:setup_org_colors() abort
--       hi OrgAgendaDeadline guifg=#FFAAAA
--       hi OrgAgendaScheduled guifg=#AAFFAA
--       hi OrgAgendaScheduledPast guifg=Orange
--     endfunction
-- ]]

local config = {
    ui = {
        theme = "catppuccin_macchiato",
        splash_animation = false,
        typewriter_ms_per_char = 0,
        mouse_scroll_lines = 5,
        show_thinking = true,
        clock_format = "24h",
    },
    plugins = {
        edit = {
            multiedit = true,
            edit_lines = true,
            insert_lines = true,
        },
    },
}

if type(maki_profile_setup) == "function" then
    maki_profile_setup(config)
end

maki.setup(config)

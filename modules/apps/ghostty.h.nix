{
  flake.modules.homeManager.ghostty = { pkgs, ... }:
    let
      mod = if pkgs.stdenv.isDarwin then "cmd" else "ctrl";
    in
    {
      programs.ghostty = {
        enable = true;
        package = pkgs.ghostty-bin;
        settings = {
          auto-update = "off";
          mouse-hide-while-typing = true;
          theme = "catppuccin-macchiato";
          window-width = 100;
          window-height = 27;
          quick-terminal-position = "top";
          quick-terminal-screen = "main";
          quick-terminal-space-behavior = "remain";
          quick-terminal-animation-duration = 0;
          bell-features = "no-audio";
          shell-integration-features = "cursor,sudo,title,ssh-env,ssh-terminfo";
          notify-on-command-finish = "unfocused";
          notify-on-command-finish-action = "notify";
          notify-on-command-finish-after = "30s";
          unfocused-split-opacity = 0.7;
          keybind = [
            "alt+k=scroll_page_up"
            "alt+j=scroll_page_down"
            "${mod}+t=new_tab"
            "${mod}+tab=next_tab"
            "global:super+period=toggle_quick_terminal"
            "global:super+ctrl+i=toggle_quick_terminal"
            "ctrl+l=clear_screen"
            "${mod}+alt+h=resize_split:left,10"
            "${mod}+alt+j=resize_split:down,10"
            "${mod}+alt+k=resize_split:up,10"
            "${mod}+alt+l=resize_split:right,10"
            "${mod}+h=goto_split:left"
            "${mod}+j=goto_split:down"
            "${mod}+k=goto_split:up"
            "${mod}+l=goto_split:right"
          ];
        };
        themes = {
          catppuccin-macchiato = {
            palette = [
              "0=#494d64"
              "1=#ed8796"
              "2=#a6da95"
              "3=#eed49f"
              "4=#8aadf4"
              "5=#f5bde6"
              "6=#8bd5ca"
              "7=#a5adcb"
              "8=#5b6078"
              "9=#ed8796"
              "10=#a6da95"
              "11=#eed49f"
              "12=#8aadf4"
              "13=#f5bde6"
              "14=#8bd5ca"
              "15=#b8c0e0"
            ];
            background = "24273a";
            foreground = "cad3f5";
            cursor-color = "f4dbd6";
            cursor-text = "24273a";
            selection-background = "3a3e53";
            selection-foreground = "cad3f5";
          };
        };
      };
    };
}

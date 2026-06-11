{
  flake.modules.homeManager.kitty = { pkgs, ... }: {
    programs.kitty = {
      enable = true;
      settings = {
        "enable_audio_bell" = "no";
        "shell" = "${pkgs.fish}/bin/fish";
      };
      keybindings = {
        "alt+k" = "scroll_page_up";
        "alt+j" = "scroll_page_down";
        "ctrl+t" = "new_tab_with_cwd";
        "ctrl+tab" = "next_tab";
      };
    };

    xdg.configFile."kitty/open-actions.conf".text = ''
      # Open any file with a fragment in nvim, fragments are generated
      # by the hyperlink_grep kitten and nothing else so far.
      protocol file
      fragment_matches [0-9]+
      action launch --type=overlay nvim +''${FRAGMENT} ''${FILE_PATH}

      # Open text files without fragments in the editor
      protocol file
      mime text/*
      action launch --type=overlay ''${EDITOR} ''${FILE_PATH}
    '';
  };
}

{ pkgs, system, ... }:

{
  programs.zathura = {
    enable = true;
    options = {
      default-bg = "#2E3440";
      default-fg = "#202020";
      statusbar-bg = "#3B4252";
      statusbar-fg = "#D8DEE9";
      inputbar-bg = "#2E3440";
      inputbar-fg = "#D8DEE9";
      notification-error-bg = "#BF616A";
      notification-error-fg = "#D8DEE9";
      notification-warning-bg = "#D08770";
      notification-warning-fg = "#2E3440";
      highlight-color = "#EBCB8B";
      highlight-active-color = "#6A9FB5";
      completion-highlight-fg = "#303030";
      completion-highlight-bg = "#75B5AA";
      completion-bg = "#303030";
      completion-fg = "#75B5AA";
      notification-bg = "#A3BE8C";
      notification-fg = "#151515";
      recolor-lightcolor = "#2E3440";
      recolor-darkcolor = "#D8DEE9";
      recolor = false;
      recolor-keephue = true;
      guioptions = "none";
      selection-clipboard = "clipboard";
    };
    extraConfig = ''
      map <C-c> set recolor-keephue "true"
      map <C-w> set recolor "true"
      map <C-t> toggle_statusbar
    '';

  };
}

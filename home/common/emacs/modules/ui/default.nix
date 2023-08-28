{ epkgs }:

{
  packages = with epkgs; [
    ligature
    doom-themes
    doom-modeline
    all-the-icons
    visual-fill-column
    minimap
    hl-todo # highlight todo and other common keyword inside the buffer
    idle-highlight-mode # highlight the word under cursor after some idle time
    svg-lib
    svg-tag-mode
  ];
}

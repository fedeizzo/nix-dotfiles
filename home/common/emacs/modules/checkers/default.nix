{ epkgs }:

{
  packages = with epkgs; [
    flycheck # flymake substitute
    flycheck-projectile # flycheck over the current project
    flycheck-posframe # show messages just below the cursor
    # flyspell-correct
    jinx # spell checking
  ];
}

{ epkgs }:

{
  packages = with epkgs; [
    cape # provides some completion function like file autocomplete
    consult # search and navigation commands based on the emacs completion function
    consult-org-roam # provides a consult command for org roam
    consult-projectile # provides a consult command that integrates very well with projectile
    corfu # completion framework that works on top of default completion functions
    embark-consult # embark applies a command based on the nearest element
    kind-icon # icon for corfu
    marginalia # marginal informations in the minibuffer
    orderless # brain behind filtering when searching for something 
    vertico # minibuffer alternative to default
    vertico-posframe # floating vertico in the center of the screen
  ];
}

{ epkgs }:

{
  packages = with epkgs; [
    hydra
    meow
    which-key
    major-mode-hydra
    meow
  ];
}

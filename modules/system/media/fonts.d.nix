{
  flake.modules.darwin.fonts = { pkgs, ... }: {
    fonts.packages = [ pkgs.nerd-fonts.jetbrains-mono ];
  };
}

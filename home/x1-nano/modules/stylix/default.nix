{ pkgs, inputs, ... }:

{
  imports = [
    inputs.stylix.homeManagerModules.stylix
  ];

  stylix = {
    enable = true;
    autoEnable = false;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
    image = ../../../common/images/wallpaper.png;
    fonts = {
      sizes = {
        applications = 12;
        desktop = 12;
        popups = 11;
        terminal = 12;
      };
      serif = {
        package = pkgs.nerd-fonts.ubuntu;
        name = "Ubuntu Nerd Font";
      };
      monospace = {
        package = pkgs.nerd-fonts.jetbrains-mono;
        name = "JetBrainsMono Nerd Font";
      };
      sansSerif = {
        package = pkgs.nerd-fonts.ubuntu-sans;
        name = "UbuntuSans Nerd Font";
      };
    };
    targets = {
      firefox.enable = true;
      firefox.profileNames = [ "fedeizzo" ];
      fish.enable = true;
      fzf.enable = true;
      gtk.enable = true;
    };
  };
}

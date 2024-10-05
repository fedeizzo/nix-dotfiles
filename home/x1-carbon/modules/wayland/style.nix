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
      serif = {
        package = inputs.apple-fonts.packages.${pkgs.system}.sf-pro-nerd;
        name = "SFProDisplay Nerd Font";
      };
    };
    targets = {
      firefox.enable = true;
      fish.enable = true;
      fzf.enable = true;
      hyprland.enable = true;
      # hyprpaper.enable = true;
      rofi.enable = true;
    };
  };
}

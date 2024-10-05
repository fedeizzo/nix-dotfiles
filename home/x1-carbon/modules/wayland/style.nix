{ pkgs, inputs, ... }:

let
  myFonts = (pkgs.nerdfonts.override {
    fonts = [
      "Ubuntu"
      "CascadiaCode"
      "UbuntuSans"
    ];
  });
in
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
        package = myFonts;
        name = "Ubuntu Nerd Font";
      };
      monospace = {
        package = myFonts;
        name = "CaskaydiaCove Nerd Font";
      };
      sansSerif = {
        package = myFonts;
        name = "UbuntuSans Nerd Font";
      };
    };
    targets = {
      firefox.enable = true;
      fish.enable = true;
      fzf.enable = true;
      hyprland.enable = true;
      # hyprpaper.enable = true;
      rofi.enable = true;
      kitty.enable = true;
      gtk.enable = true;
    };
  };
}

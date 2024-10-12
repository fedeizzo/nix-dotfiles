{ inputs, pkgs, ... }:

let
  myFonts = [
    (pkgs.nerdfonts.override {
      fonts = [
        "Ubuntu"
        "CascadiaCode"
        "UbuntuSans"
      ];
    })
    pkgs.font-awesome
    pkgs.joypixels
    pkgs.emacs-all-the-icons-fonts
  ];
in
{
  imports = [
    inputs.stylix.homeManagerModules.stylix
  ];

  fonts = {
    packages = [ myFonts ];
  };

  stylix = {
    enable = true;
    autoEnable = false;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
    image = ../../../common/images/wallpaper.png;
    fonts = {
      packages = [ myFonts ];
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
      nixos-icons.enable = true;
      grub.enable = true;
      grub.useImage = true;
    };
  };
}

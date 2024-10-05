{ inputs, pkgs, ... }:

{
  imports = [
    inputs.stylix.homeManagerModules.stylix
  ];

  fonts = {
    packages = [
      pkgs.font-awesome
      pkgs.joypixels
      pkgs.emacs-all-the-icons-fonts
    ];
  };

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
      nixos-icons.enable = true;
      grub.enable = true;
      grupb.useImage = true;
    };
  };

}

{ pkgs, inputs, ... }:

{
  imports = [
    inputs.stylix.nixosModules.sytlix
  ];

  sytlix = {
    enable = true;
    autoEnable = false;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
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
      hyprpaper.enable = true;
      rofi.enable = true;
    };
  };
}

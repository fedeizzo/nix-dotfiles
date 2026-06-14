{
  flake.modules.nixos.x1-nano = { inputs, pkgs, ... }:
    let
      myFonts = [
        pkgs.nerd-fonts.ubuntu
        pkgs.nerd-fonts.jetbrains-mono
        pkgs.nerd-fonts.ubuntu-sans
        pkgs.font-awesome
        pkgs.joypixels
        pkgs.emacs-all-the-icons-fonts
      ];
    in
    {
      fonts = {
        packages = myFonts;
      };

      stylix = {
        enable = true;
        autoEnable = false;
        base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
        image = ../../../../assets/wallpaper.png;
        fonts = {
          packages = myFonts;
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
          nixos-icons.enable = true;
          grub.enable = true;
          grub.useImage = true;
        };
      };

      imports = [
        inputs.stylix.nixosModules.stylix
      ];
    };

  flake.modules.homeManager.stylix = {
    stylix.targets = {
      firefox.enable = true;
      firefox.profileNames = [ "fedeizzo" ];
      fish.enable = true;
      fzf.enable = true;
      gtk.enable = true;
    };
  };
}

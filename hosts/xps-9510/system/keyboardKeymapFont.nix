{ pkgs, ... }:

{
  imports = [
    ../../common/system/keyboardKeymapFont.nix
  ];

  services = {
    keyd = {
      enable = true;
      keyboards = {
        default = {
          ids = [ "0001:0001" ];
          settings = {
            main = {
              capslock = "overload(control, leftcontrol)";
              leftcontrol = "capslock";
            };
          };
        };
      };
    };
    fprintd = {
      enable = true;
      package = pkgs.fprintd;
      tod = {
        enable = true;
        driver = pkgs.libfprint-2-tod1-goodix;
      };
    };
  };

  fonts = {
    packages = [
      pkgs.fira-code
      pkgs.font-awesome
      pkgs.joypixels
      pkgs.symbola
      pkgs.jetbrains-mono
      pkgs.emacs-all-the-icons-fonts
      (pkgs.nerdfonts.override {
        fonts = [
          "Meslo"
          "RobotoMono"
        ];
      })
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
      };
    };
  };
}

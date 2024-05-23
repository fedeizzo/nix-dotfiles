{ pkgs, nixpkgs-old, ... }:

{
  services.keyd = {
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
  services = {
    fprintd = {
      enable = true;
      package = nixpkgs-old.fprintd;
      tod = {
        enable = true;
        driver = pkgs.libfprint-2-tod1-goodix;
      };
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    keyMap = "us";
  };
  time.timeZone = "Europe/Rome";
  time.hardwareClockInLocalTime = false;

  fonts = {
    packages = [
      pkgs.fira-code
      pkgs.font-awesome
      pkgs.joypixels
      pkgs.symbola
      pkgs.jetbrains-mono
      pkgs.emacs-all-the-icons-fonts
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
      };
    };
  };
}

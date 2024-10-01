{ pkgs, ... }:

{
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
  }

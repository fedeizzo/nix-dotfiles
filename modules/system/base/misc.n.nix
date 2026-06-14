{
  flake.modules.nixos.misc = {
    i18n.defaultLocale = "en_US.UTF-8";
    console.keyMap = "us";
    time.timeZone = "Europe/Paris";

    programs.ccache.enable = true;
  };
}

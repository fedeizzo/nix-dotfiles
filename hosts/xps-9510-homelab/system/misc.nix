_:

{
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    keyMap = "us";
  };
  time.timeZone = "Europe/Paris";
  time.hardwareClockInLocalTime = true;

  programs.ccache.enable = true;

  virtualisation = {
    docker = {
      enable = true;
      # enableNvidia = true;
      enableOnBoot = true;
      rootless = {
        enable = true;
        setSocketVariable = true;
      };
      autoPrune = {
        enable = true;
        flags = [ ];
        dates = "weekly";
      };
    };
  };
}

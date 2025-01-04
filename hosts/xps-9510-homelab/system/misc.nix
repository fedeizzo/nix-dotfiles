{ inputs, ... }:

{
  imports = [ inputs.nur.modules.nixos.default ];
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    keyMap = "us";
  };
  time.timeZone = "Europe/Paris";
  time.hardwareClockInLocalTime = true;

  programs.ccache.enable = true;

  virtualisation = {
    oci-containers.backend = "docker";
    podman.enable = false;
    docker = {
      enable = true;
      # enableNvidia = true;
      enableOnBoot = true;
      autoPrune = {
        enable = true;
        flags = [ ];
        dates = "weekly";
      };
    };
  };
}

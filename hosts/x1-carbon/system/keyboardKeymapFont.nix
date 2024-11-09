{ inputs, ... }:

{
  imports = [

    inputs.nixos-06cb-009a-fingerprint-sensor.nixosModules.open-fprintd
    inputs.nixos-06cb-009a-fingerprint-sensor.nixosModules.python-validity
  ];

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    keyMap = "us";
  };
  time.timeZone = "Europe/Paris";
  time.hardwareClockInLocalTime = false;

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
    fprintd.enable = false;
    open-fprintd.enable = true;
    python-validity.enable = true;
  };

  systemd.services.fix-fingerprint-suspend = {
    enable = true;

    after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" "suspend-then-hibernate.target" ];
    wantedBy = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" "suspend-then-hibernate.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "systemctl restart open-fprintd.service python3-validity.service";
    };
  };

  security.pam.services = {
    doas.fprintAuth = true;
  };
}

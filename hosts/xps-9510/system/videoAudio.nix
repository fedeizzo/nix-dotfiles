{ pkgs, ... }:

{
  imports = [
    ../../common/system/videoAudio.nix
  ];

  services.clight = {
    enable = true;
    settings = {
      inhibit = {
        disabled = false;
        inhibit_docked = true;
        inhibit_pm = true;
        inhibit_bl = true;
      };

      backlight = {
        pause_on_lid_closed = true;
        capture_on_lid_opened = true;
      };

      sensor = {
        # ac_regression_points = [ 0.0 0.30 0.40 0.45 0.61 0.74 0.81 0.88 0.93 0.97 1.0 ];
        # batt_regression_points = [ 0.0 0.30 0.40 0.45 0.61 0.74 0.81 0.88 0.93 0.97 1.0 ];
        devname = "/dev/iio:device0";
      };

      daytime = {
        latitude = 48.8575;
        longitude = 2.3514;
      };
    };
  };
}

{ pkgs, ... }:

{
  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };
  home.packages = [ pkgs.libreoffice ];
}

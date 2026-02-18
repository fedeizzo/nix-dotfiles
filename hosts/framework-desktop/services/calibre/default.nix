{ ... }:

{
  services.calibre-web = {
    enable = true;
    user = "calibre-server";
    group = "calibre-server";
    listen.port = 44533;
    openFirewall = true;
    options = {
      calibreLibrary = "/var/lib/calibre-web";
      enableBookConversion = true;
      enableBookUploading = true;
      enableKepubify = true;
    };
  };
}

{ ... }:

{
  users.users.calibre-web.guid = 206; # make backup consistent across machines
  users.users.calibre-web.uid = 206; # make backup consistent across machines

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

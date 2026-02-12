{ ... }:

{
  users.groups.calibre-server.gid = 206; # make backup consistent across machines
  users.users.calibre-server.uid = 206; # make backup consistent across machines
  users.users.calibre-server.group = "calibre-server";

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

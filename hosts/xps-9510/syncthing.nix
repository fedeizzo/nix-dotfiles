{ pkgs, username, ... }:

{
  services.syncthing = {
    enable = true;
    systemService = true;
    openDefaultPorts = true;
    overrideDevices = false;
    user = "${username}";
    group = "users";
    dataDir = "/home/${username}";
    extraOptions = {
      gui = {
        theme = "black";
      };
    };
    folders = {
      "OrgAgenda" = {
        path = "/home/${username}/org";
        watch = true;
        type = "sendreceive";
      };
    };
  };
}

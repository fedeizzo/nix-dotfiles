{ config, pkgs, libs, ... }:

{
  programs.git = {
    enable = true;
    userName = "Federico Izzo";
    userEmail = "federico.izzo99@gmail.com";
    signing = {
      key = "federico.izzo99@gmail.com";
      signByDefault = true;
    };
    lfs.enable = true;
    extraConfig = {
      init = {
        defaultBranch = "master";
      };
    };
    aliases = {
      co = "checkout";
      br = "branch";
      ci = "commit -s";
      cim = "commit -s -m";
      cin = "commit -s -n";
      st = "status";
      aa = "add .";
      a = "add";
      pl = "pull";
      l = "log --graph --name-status";
      ps = "push";
      psu = "push -u origin HEAD";
      psf = "push --force-with-lease";
      me = "merge";
    };
  };
}

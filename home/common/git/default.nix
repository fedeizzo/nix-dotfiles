{ config, ... }:

{
  programs.git = {
    enable = true;
    userName = "Federico Izzo";
    userEmail = "federico@fedeizzo.dev";
    # signing = {
    #   key = "7F857F72";
    #   signByDefault = true;
    # };
    lfs.enable = true;
    extraConfig = {
      init = {
        defaultBranch = "master";
      };
      safe = {
        directory = "${config.home.homeDirectory}/nix-dotfiles";
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

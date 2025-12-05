{ config, ... }:

{
  programs.git = {
    enable = true;
    # signing = {
    #   key = "7F857F72";
    #   signByDefault = true;
    # };
    lfs.enable = true;
    settings = {
      user.name = "Federico Izzo";
      user.email = "federico@fedeizzo.dev";
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
      init = {
        defaultBranch = "main";
      };
      safe = {
        directory = "${config.home.homeDirectory}/nix-dotfiles";
      };
    };
  };
}

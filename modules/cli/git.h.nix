{
  flake.modules.homeManager.git = { pkgs, config, ... }: {
    programs.git = {
      enable = true;
      lfs.enable = true;
      settings = {
        user.name = "Federico Izzo";
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
  };
}

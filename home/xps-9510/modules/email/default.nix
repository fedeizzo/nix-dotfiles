{ pkgs, pkgs-unstable, config, ... }:

{
  home.packages = [ pkgs.mailcap ];
  home.file.".mailcap" = {
    text = ''
      
            audio/*; ${pkgs.mpv}/bin/mpv add %s
            image/*; hyprctl keyword windowrulev2 "float, class:^(imv)\$" && ${pkgs.imv}/bin/imv %s & && disown && sleep 1 && hyprctl keyword windowrulev2 "unset,class:^(imv)\$"

            application/msword; ${pkgs.xdg-utils}/bin/xdg-open %s
            application/pdf; hyprctl keyword windowrulev2 "float, class:^(org.pwmt.zathura)\$" && ${pkgs.zathura}/bin/zathura %s & && disown && sleep 1 && hyprctl keyword windowrulev2 "unset,class:^(org.pwmt.zathura)\$"
            application/postscript; hyprctl keyword windowrulev2 "float, class:^(org.pwmt.zathura)\$" && ${pkgs.zathura}/bin/zathura %s & && disown && sleep 1 && hyprctl keyword windowrulev2 "unset,class:^(org.pwmt.zathura)\$"

            text/html; ${pkgs.xdg-utils}/bin/xdg-open %s

    '';
  };
  accounts.email.maildirBasePath = ".mail";
  accounts.email.accounts = {
    fedeizzo = {
      primary = true;
      realName = "Federico Izzo";
      address = "federico.izzo99@gmail.com";
      flavor = "gmail.com";
      maildir.path = "fedeizzo";
      lieer = {
        enable = true;
        notmuchSetupWarning = true;
        # settings = { };
        sync.enable = true;
        sync.frequency = "*:0/5"; # every five minutes
      };
      notmuch.enable = true;
    };
    ozzi = {
      primary = false;
      realName = "Federico Izzo";
      address = "ozzi.ezzo@gmail.com";
      flavor = "gmail.com";
      maildir.path = "ozzi";
      lieer = {
        enable = true;
        notmuchSetupWarning = true;
        # settings = { };
        sync.enable = true;
        sync.frequency = "*:0/5"; # every five minutes
      };
      notmuch.enable = true;
    };
    uni = {
      primary = false;
      realName = "Federico Izzo";
      address = "federico.izzo@studenti.unitn.it";
      flavor = "gmail.com";
      maildir.path = "uni";
      lieer = {
        enable = true;
        notmuchSetupWarning = true;
        # settings = { };
        sync.enable = true;
        sync.frequency = "*:0/5"; # every five minutes
      };
      notmuch.enable = true;
    };
  };
  programs.notmuch = {
    enable = true;
    hooks.preNew = ''
      gmi sync -C ~/.mail/fedeizzo/
      gmi sync -C ~/.mail/ozzi/
      gmi sync -C ~/.mail/uni/
    '';
  };
  programs.lieer.enable = true;
  services.lieer.enable = true;
}

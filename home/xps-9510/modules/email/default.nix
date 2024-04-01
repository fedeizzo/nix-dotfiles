{ pkgs, pkgs-unstable, sops, ... }:

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
    federico = {
      address = "federico@fedeizzo.dev";
      userName = "federico@fedeizzo.dev";
      passwordCommand = "cat /dev/null";
      primary = true;
      realName = "Federico Izzo";
      imap = {
        host = "imap.fastmail.com";
        port = 993;
        tls = {
          enable = true;
        };
      };
      smtp = {
        host = "smtp.fastmail.com";
        port = 465;
        tls = {
          enable = true;
        };
      };
      mbsync = {
        enable = true;
        create = "both";
        extraConfig = {
          channel = {
            Sync = "All";
            CopyArrivalDate = "yes";
          };
        };
      };
      msmtp.enable = true;
      mu.enable = true;
    };
    # university = {
    #   primary = false;
    #   realName = "Federico Izzo";
    #   address = "federico.izzo@studenti.unitn.it";
    #   flavor = "gmail.com";
    #   maildir.path = "university";
    #   lieer = {
    #     enable = true;
    #     notmuchSetupWarning = true;
    #     # settings = { };
    #     sync.enable = true;
    #     sync.frequency = "*:0/5"; # every five minutes
    #   };
    #   mu.enable = true;
    # };
  };
  programs.mu.enable = true;
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  services.mbsync.enable = true;
  services.mbsync.frequency = "*:0/5";
  # programs.lieer.enable = true;
  # services.lieer.enable = true;
}

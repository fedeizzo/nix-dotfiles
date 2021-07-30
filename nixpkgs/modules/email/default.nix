{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    email-sync
    libsecret
  ];
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      Personal = {
        primary = true;
        address = "federico.izzo99@gmail.com";
        passwordCommand = "${pkgs.gnome3.libsecret}/bin/secret-tool lookup appPasswordGooglePrimario password";
        realName = "Federico Izzo";
        flavor = "gmail.com";

        imapnotify = {
          enable = true;
          boxes = [ "Inbox" ];
          onNotify = "/usr/bin/env";
          onNotifyPost = {
            mail = "${pkgs.notmuch}/bin/notmuch new && ${pkgs.libnotify}/bin/notify-send 'New mail arrived'";
          };
        };
        mbsync = {
          enable = true;
          patterns = [ "*" ];
          create = "both";
        };

        msmtp.enable = true;
        notmuch.enable = true;
      };
    };
  };
  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
    notmuch.enable = true;
    alot = {
      enable = true;
      bindings = {
        global = {
          T = "search tag:todo";
        };
        search = {
          t = "toggletags todo";
          D = "toggletags killed";
        };
        thread = {
          B = "call hooks.open_in_browser(ui)";
        };
        envelope = {
          B = "call hooks.open_in_browser(ui)";
        };
      };
      hooks = builtins.readFile ./alot_hooks.py;
    };
    afew = {
      enable = true;
      extraConfig = builtins.readFile ./afew.cfg;
    };
  };
  services.mbsync = {
    enable = true;
    frequency = "*:0/5"; # every 5m
    postExec = "${pkgs.email-sync}/bin/email-sync";
  };
  home.file = {
    ".mailcap".text = ''
      text/html;  w3m -dump -o document_charset=%{charset} '%s'; nametemplate=%s.html; copiousoutput
    '';
  };
}

{ pkgs, ... }:

{
  programs.fish = {
    enable = true;
    shellAliases = {
      ls = "exa --icons --sort=type";
      ll = "exa -l --icons --sort=type";
      lll = "exa -l --icons --sort=type | less";
      lla = "exa -la --icons --sort=type";
      llt = "exa -T --icons --sort=type";
      vi = "nvim";
      SS = "systemctl";
      it = "setxkbmap it && xmodmap $HOME/.Xmodmap.back";
      us = "setxkbmap -layout us -variant altgr-intl && xmodmap $HOME/.Xmodmap.back";
      streamlink = "streamlink -p 'devour vlc'";
      vlc = "devour vlc";
      zathura = "devour zathura";
      llpp = "devour llpp";
      sxiv = "devour sxiv";
      v = "nvim";
      open = "xdg-open";
      cat = "bat";
      gt = "gitui";
      gs = "git status";
      ga = "git add -A";
      gc = "git commit -m";
      gp = "git push";
      noblackscreen = "xset s off -dpms";
    };
    shellAbbrs = {
      "..." = "cd ../..";
      "...." = "cd ../../..";
      "....." = "cd ../../../..";
      "ssh" = "TERM=xterm-256color ssh";
    };
    promptInit = ''
      eval (starship init fish)
      fish_vi_key_bindings
      set -gx PROJECT_PATHS ~/fbk ~/personalProject
      set -U __done_min_cmd_duration 5000
      set fish_color_command A3BE8C
      set fish_greeting
    '';
    plugins = [
      {
        name = "done";
        src = pkgs.fetchFromGitHub {
          owner = "franciscolourenco";
          repo = "done";
          rev = "1.15.0";
          sha256 = "1i7k59kjik41b7mww6d1qbi66vswplmvjdscinyf60irbrsbc5bv";
        };
      }
      {
        name = "pj";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-pj";
          rev = "b99b1df1b1dc10ef94ce8376661a7307113a311a";
          sha256 = "1awwp26xwl5sfqhzrklyf2552xg4y89450g15fk5zvq2j2q2mcz8";
        };
      }
      {
        name = "bang-bang";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-bang-bang";
          rev = "f969c618301163273d0a03d002614d9a81952c1e";
          sha256 = "1r3d4wgdylnc857j08lbdscqbm9lxbm1wqzbkqz1jf8bgq2rvk03";
        };
      }
      {
        name = "plugin-extract";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-extract";
          rev = "5d05f9f15d3be8437880078171d1e32025b9ad9f";
          sha256 = "0cagh2n5yg8m6ggzhf3kcp714gb8s7blb840kxas0z6366w3qlw4";
        };
      }
    ];
  };
}
